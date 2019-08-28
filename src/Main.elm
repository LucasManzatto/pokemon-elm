module Main exposing (..)

import Bootstrap.CDN as CDN
import Browser
import Debug exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as Decode exposing (Decoder, bool, int, list, string)
import Json.Decode.Pipeline exposing (custom, hardcoded, required)
import List.Extra as List
import Dict exposing (Dict)



---- MODEL ----


type alias Model =
    { pokemons : List Pokemon
    , newPokemon : String
    , error : String
    }


type alias Pokemon =
    { id : Int
    , name : String
    , types : (List String)
    }

pokemonTypes : Dict String String
pokemonTypes =
    Dict.fromList
        [ ("fire", "#FF0000")
        , ("water", "#00BFFF")
        ]

pokemonDecoder : Decoder Pokemon
pokemonDecoder =
    Decode.succeed Pokemon
        |> required "id" int
        |> required "name" string
        |> required "types" (list string)

pokemonListDecoder : Decoder (List Pokemon)
pokemonListDecoder =
    Decode.list pokemonDecoder


init : ( Model, Cmd Msg )
init =
    ( { pokemons = [], newPokemon = "", error = "" }
    , Http.get
        { url = "http://localhost:58803/api/Pokemons"
        , expect = Http.expectJson PokemonsReceived pokemonListDecoder
        }
    )



-- UPDATE ----


type Msg
    = UpdatePokemonInput String
    | PokemonsReceived (Result Http.Error (List Pokemon))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdatePokemonInput newInput ->
            ( { model | newPokemon = newInput }, Cmd.none )

        PokemonsReceived (Ok receivedPokemons) ->
            ( { model | pokemons = receivedPokemons }, Cmd.none )

        PokemonsReceived (Err httpErr) ->
            log (toString msg)
                ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ CDN.stylesheet
        , div [ class "row" ]
            [ div [ class "col" ]
                [ navbar ]
            ]
        , div [ class "row" ]
            (List.map pokemon model.pokemons)
        ]


pokemon : Pokemon -> Html Msg
pokemon pokemonRecord =
    div [ class "col-2 text-center" ]
        [ img [ class "img-fluid", style "margin" "0", src (String.concat [ "https://img.pokemondb.net/sprites/omega-ruby-alpha-sapphire/dex/normal/", String.toLower pokemonRecord.name, ".png" ]) ] []
        , button [class "btn btn-link"] [text (capitalize True pokemonRecord.name)]
        , div [class "row justify-content-center"]
            (List.map pokemonType pokemonRecord.types)
        ]

pokemonType : String -> Html Msg
pokemonType name =
    div [class "col-6", style "color" (getColorFromType name)
        ] [text name]


navbar : Html Msg
navbar =
    nav [ class "navbar navbar-light bg-light justify-content-start" ]
        [ span [ class "navbar-brand mb-0 h1" ] [ text "Navbar" ]
        , ul [ class "navbar-nav" ]
            [ li [ class "nav-item" ]
                [ a [ class "nav-link", href "#" ] [ text "Pokédex" ]
                ]
            ]
        ]



---- PROGRAM ----

getColorFromType : String -> String
getColorFromType typeName =
    case Dict.get typeName pokemonTypes of
        Just color ->
            color
        Nothing ->
            "#000000"


capitalize : Bool -> String -> String
capitalize shouldCapitalize str =
  case String.uncons str of
    Nothing ->
      str

    Just ( firstLetter, rest ) ->
      let
        newFirstLetter =
          if shouldCapitalize then
            Char.toUpper firstLetter
          else
            Char.toLower firstLetter
      in
        String.cons newFirstLetter rest

main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
