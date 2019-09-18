module Pages.Pokedex exposing (Model, Msg, init, update, view)

import Api as Api exposing (..)
import Browser exposing (Document, UrlRequest(..))
import Debug exposing (log)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (get)
import Json.Decode as Decode exposing (list)
import List.Extra as List
import Pages.Shared as Shared exposing (..)
import Platform.Cmd exposing (Cmd)
import Regex exposing (replace)
import Types exposing (..)
import Url exposing (Url)
import Utils exposing (..)


type alias Model =
    { pokemons : List Pokemon
    }


getPokedex : Cmd Msg
getPokedex =
    Http.get
        { url = apiUrl ++ "/Pokemons"
        , expect = Http.expectJson (fromResult >> PokemonsReceived) (Decode.list pokemonDecoder)
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { pokemons = []
      }
    , getPokedex
    )



--------------- UPDATE ----------------------


type Msg
    = PokemonsReceived (WebData (List Pokemon))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PokemonsReceived response ->
            case response of
                NotAsked ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Success pokemonsList ->
                    ( { model | pokemons = pokemonsList }, Cmd.none )

                Failure _ ->
                    ( model, Cmd.none )


view : Model -> Html msg
view model =
    let
        pokemonsGroupedByGeneration =
            List.gatherEqualsBy .generation model.pokemons

        pokedexGens =
            List.map pokedexGeneration pokemonsGroupedByGeneration
    in
    div [ class "row" ]
        pokedexGens


pokedexGeneration : ( Pokemon, List Pokemon ) -> Html msg
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
            ((firstPokemonFromGen :: genList)
                |> List.map
                    (\poke ->
                        div [ class "text-center", style "width" "12.5%" ]
                            (Shared.pokemonCard poke)
                    )
            )
        ]
