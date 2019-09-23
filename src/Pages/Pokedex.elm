module Pages.Pokedex exposing (Model, Msg, init, initPokedex, subscriptions, update, view)

import Animation exposing (px)
import Api as Api exposing (..)
import Browser exposing (Document, UrlRequest(..))
import Debug exposing (log)
import Delay as Delay exposing (..)
import Dict exposing (..)
import Ease
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import List.Extra as List
import Pages.Shared exposing (..)
import Pages.SinglePokemon as SinglePokemonPage exposing (..)
import Platform.Cmd exposing (Cmd)
import Regex exposing (replace)
import Time as Time exposing (..)
import Transit
import Types exposing (..)
import Url exposing (Url)
import Utils exposing (..)
import Ionicon


type alias Model =
    { pokemons : List Pokemon
    , filteredPokemons : List Pokemon
    , selectedPokemon : Maybe SinglePokemon
    , slideValues : List Float
    , time : Time.Posix
    , openedSinglePokemonPage : Bool
    , maxStats : WebData PokemonStats
    }


type SinglePokemon
    = SinglePokemon SinglePokemonPage.Model

blackColor : RGBA
blackColor =
    RGBA 0 0 0 1


initPokedex : Model
initPokedex =
    { pokemons = []
    , filteredPokemons = []
    , selectedPokemon = Nothing
    , slideValues = List.map (\i -> Ease.inQuad (toFloat i / 10)) (List.range 0 10)
    , time = Time.millisToPosix 0
    , openedSinglePokemonPage = False
    , maxStats = NotAsked
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initPokedex
    , Cmd.batch [ Cmd.map GotApiMsg Api.getPokedex ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--------------- UPDATE ----------------------


type Msg
    = GotApiMsg Api.Msg
    | ClickedPokemon Int
    | GotSinglePokemonMsg SinglePokemonPage.Msg
    | CloseSinglePokemon
    | OpenSinglePokemon
    | FilterPokedex String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotApiMsg apiMsg ->
            case apiMsg of
                PokemonsReceived response ->
                    case response of
                        Success pokemonsList ->
                            SinglePokemonPage.init () 1
                                |> updateWith SinglePokemon GotSinglePokemonMsg { model | pokemons = pokemonsList , filteredPokemons = pokemonsList}

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotSinglePokemonMsg singlePokemonMsg ->
            case model.selectedPokemon of
                Just (SinglePokemon singlePokemonModel) ->
                    SinglePokemonPage.update singlePokemonMsg { singlePokemonModel | maxStats = model.maxStats }
                        |> updateWith SinglePokemon GotSinglePokemonMsg model

                _ ->
                    ( model, Cmd.none )

        ClickedPokemon id ->
            SinglePokemonPage.init () id
                |> updateWith SinglePokemon GotSinglePokemonMsg model

        OpenSinglePokemon ->
            ( { model | openedSinglePokemonPage = True }, Cmd.none )

        CloseSinglePokemon ->
            ( { model | openedSinglePokemonPage = False }, Cmd.none )

        FilterPokedex input ->
            let
                filteredPokedex =
                    List.filter(\pokemon -> String.contains input pokemon.name ) model.pokemons
            in
            ({model | filteredPokemons = filteredPokedex},Cmd.none)
            


updateWith : (subModel -> SinglePokemon) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( { model | selectedPokemon = Just (toModel subModel) }
    , Cmd.batch [ Cmd.map toMsg subCmd, Delay.after 0 Millisecond OpenSinglePokemon ]
    )


view : Model -> Html Msg
view model =
    let
        pokemonsGroupedByGeneration =
            List.gatherEqualsBy .generation model.filteredPokemons

        pokedexGens =
            pokemonsGroupedByGeneration
                |> List.map (\pokemonGroup -> pokedexGeneration model.openedSinglePokemonPage pokemonGroup)

        singlePokemonModel =
            case model.selectedPokemon of
                Just (SinglePokemon subModel) ->
                    subModel

                Nothing ->
                    SinglePokemonPage.initModel

        ( showPokedex, showPokemon, pokedexOverflow ) =
            if model.openedSinglePokemonPage then
                ( class "pokedex-open", class "single-pokemon-open", style "overflow" "auto" )

            else
                ( class "pokedex", class "single-pokemon", style "overflow" "initial" )

        --    style "overflow" "auto" ,style "height" "94vh"
    in
    div [ class "row justify-content-center", style "overflow" "hidden" ]
        [ div
            [ class "col-6", style "overflow" "auto", style "height" "94vh" ]
            (List.append [ searchPokemon ]
                pokedexGens
            )
        , div [ class "col-6", showPokemon, style "overflow" "auto", style "height" "94vh" ]
            [ div [ class "row" ]
                [ button
                    [ class "btn btn-link", onClick CloseSinglePokemon ]
                    [ h2 [] [ Ionicon.close 32  blackColor ] ]
                ]
            , SinglePokemonPage.view singlePokemonModel
            ]
        ]

searchPokemon : Html Msg
searchPokemon =
    div 
    [ class "row" ] 
    [ input [class "form-control", placeholder "Search Pokemon...", onInput FilterPokedex] []]


pokedexGeneration : Bool -> ( Pokemon, List Pokemon ) -> Html Msg
pokedexGeneration openedSinglePokemonPage ( firstPokemonFromGen, genList ) =
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
                                if openedSinglePokemonPage then
                                    "25%"

                                else
                                    "12.5%"
                        in
                        div [ class "text-center", style "width" "25%" ]
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
            , onClick (ClickedPokemon pokemonRecord.id)
            ]
            [ text (capitalize pokemonRecord.name) ]
        ]
    , div []
        [ types ]
    ]
