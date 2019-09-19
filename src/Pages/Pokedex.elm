module Pages.Pokedex exposing (Model, Msg, init, update, view)

import Api as Api exposing (..)
import Browser exposing (Document, UrlRequest(..))
import Debug exposing (log)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra as List
import Pages.Shared as Shared exposing (..)
import Pages.SinglePokemon as SinglePokemonPage exposing (..)
import Platform.Cmd exposing (Cmd)
import Regex exposing (replace)
import Route as Route exposing (..)
import Types exposing (..)
import Url exposing (Url)
import Utils exposing (..)


type alias Model =
    { pokemons : List Pokemon
    , selectedPokemon : Maybe SinglePokemon
    }


type SinglePokemon
    = SinglePokemon SinglePokemonPage.Model


init : () -> ( Model, Cmd Msg )
init _ =
    ( { pokemons = []
      , selectedPokemon = Nothing
      }
    , Cmd.map GotApiMsg Api.getPokedex
    )



--------------- UPDATE ----------------------


type Msg
    = GotApiMsg Api.Msg
    | ClickedPokemon Int
    | GotPokemonMsg SinglePokemonPage.Msg


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
                |> List.map (\pokemonGroup -> pokedexGeneration pokemonGroup)

        viewWithSelectedPokemon =
            case model.selectedPokemon of
                Just (SinglePokemon selectedPokemon) ->
                    div [ class "row" ]
                        [ div [ class "col-6" ] pokedexGens
                        , div [ class "col-6" ] [ SinglePokemonPage.view selectedPokemon ]
                        ]

                Nothing ->
                    div [ class "row" ]
                        pokedexGens
    in
    viewWithSelectedPokemon


pokedexGeneration : ( Pokemon, List Pokemon ) -> Html Msg
pokedexGeneration ( firstPokemonFromGen, genList ) =
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
                        div [ class "text-center", style "width" "12.5%" ]
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
