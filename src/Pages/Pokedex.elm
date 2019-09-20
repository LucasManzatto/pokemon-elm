module Pages.Pokedex exposing (Model, Msg, init, initPokedex, subscriptions, update, view)

import Api as Api exposing (..)
import Browser exposing (Document, UrlRequest(..))
import Browser.Events exposing (onAnimationFrame)
import Debug exposing (log)
import Dict exposing (..)
import Ease
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra as List
import Pages.Shared as Shared exposing (..)
import Pages.SinglePokemon as SinglePokemonPage exposing (..)
import Platform.Cmd exposing (Cmd)
import Regex exposing (replace)
import Route as Route exposing (..)
import Task
import Time as Time exposing (..)
import Transit
import TransitStyle
import Types exposing (..)
import Url exposing (Url)
import Utils exposing (..)


type alias Model =
    Transit.WithTransition
        { pokemons : List Pokemon
        , selectedPokemon : Maybe SinglePokemon
        , slideValues : List Float
        , time : Time.Posix
        }


type SinglePokemon
    = SinglePokemon SinglePokemonPage.Model


initPokedex : Model
initPokedex =
    { pokemons = []
    , selectedPokemon = Nothing
    , slideValues = List.map (\i -> Ease.inQuad (toFloat i / 10)) (List.range 0 10)
    , time = Time.millisToPosix 0
    , transition = Transit.empty
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initPokedex
    , Cmd.batch [ Cmd.map GotApiMsg Api.getPokedex ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Transit.subscriptions TransitMsg model



--------------- UPDATE ----------------------


type Msg
    = GotApiMsg Api.Msg
    | ClickedPokemon Int
    | GotPokemonMsg SinglePokemonPage.Msg
    | Slide Time.Posix
    | TransitMsg (Transit.Msg Msg)
    | Click Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotApiMsg apiMsg ->
            case apiMsg of
                PokemonsReceived response ->
                    case response of
                        Success pokemonsList ->
                            ( { model | pokemons = pokemonsList }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotPokemonMsg subMsg ->
            case model.selectedPokemon of
                Just (SinglePokemon selectedPokemon) ->
                    SinglePokemonPage.update subMsg selectedPokemon
                        |> updateWith SinglePokemon GotPokemonMsg model

                _ ->
                    ( model, Cmd.none )

        ClickedPokemon id ->
            SinglePokemonPage.init () id
                |> updateWith SinglePokemon GotPokemonMsg model

        Slide newTime ->
            ( { model | time = newTime }, Cmd.none )

        Click id ->
            Transit.start TransitMsg (ClickedPokemon id) ( 100, 200 ) model

        TransitMsg a ->
            Transit.tick TransitMsg a model


updateWith : (subModel -> SinglePokemon) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( { model | selectedPokemon = Just (toModel subModel) }
    , Cmd.map toMsg subCmd
    )


view : Model -> Html Msg
view model =
    let
        pokemonsGroupedByGeneration =
            List.gatherEqualsBy .generation model.pokemons

        pokedexGens =
            pokemonsGroupedByGeneration
                |> List.map (\pokemonGroup -> pokedexGeneration model.selectedPokemon pokemonGroup)

        viewWithSelectedPokemon =
            let
                ( showPokemon, selectedPokemon ) =
                    case model.selectedPokemon of
                        Just (SinglePokemon poke) ->
                            ( style "display" "initial", poke )

                        Nothing ->
                            ( style "display" "none", SinglePokemonPage.initModel )
            in
            div (showPokemon :: class "col" :: TransitStyle.fadeSlide 50 model.transition) [ SinglePokemonPage.view selectedPokemon ]
    in
    div [ class "row" ]
        [ div [ class "col" ] pokedexGens
        , viewWithSelectedPokemon
        ]


pokedexGeneration : Maybe SinglePokemon -> ( Pokemon, List Pokemon ) -> Html Msg
pokedexGeneration selectedPokemon ( firstPokemonFromGen, genList ) =
    let
        generation =
            firstPokemonFromGen.generation
                |> String.replace "-" " "
                |> String.split " "

        formatedGen =
            case generation of
                [ first, last ] ->
                    capitalize first ++ " " ++ String.toUpper last

                _ ->
                    ""

        viewPokemonCards =
            (firstPokemonFromGen :: genList)
                |> List.map
                    (\poke ->
                        let
                            pokemonsInRow =
                                case selectedPokemon of
                                    Just (SinglePokemon _) ->
                                        "25%"

                                    Nothing ->
                                        "12.5%"
                        in
                        div [ class "text-center", style "width" pokemonsInRow ]
                            (pokemonCard poke)
                    )
    in
    div [ class "col-12" ]
        [ div
            [ class "row text-center mt-3 font-weight-bold"
            , style "position" "sticky"
            , style "top" "0"
            , style "background" "white"
            , style "z-index" "10"
            ]
            [ div [ class "col" ]
                [ text formatedGen ]
            ]
        , div [ class "row" ]
            viewPokemonCards
        ]


pokemonCard : Pokemon -> List (Html Msg)
pokemonCard pokemonRecord =
    let
        formatedPokemonNumber =
            "#" ++ String.padLeft 3 '0' (String.fromInt pokemonRecord.id)

        types =
            case pokemonRecord.types of
                [ firstType, secondType ] ->
                    div [ class "row justify-content-center no-gutters" ]
                        [ pokemonTypeLink firstType
                        , text " - "
                        , pokemonTypeLink secondType
                        ]

                [ justFirst ] ->
                    div [ class "row justify-content-center no-gutters" ]
                        [ div [ class "col-4" ] [ pokemonTypeLink justFirst ]
                        ]

                _ ->
                    text ""
    in
    [ div []
        [ h2 [] []
        , pokemonImgSprite pokemonRecord.name pokemonRecord.generation
        ]
    , div []
        [ text formatedPokemonNumber ]
    , div []
        [ button
            [ style "padding" "0"
            , class "btn btn-link font-weight-bold"
            , onClick (Click pokemonRecord.id)
            ]
            [ text (capitalize pokemonRecord.name) ]
        ]
    , div []
        [ types ]
    ]
