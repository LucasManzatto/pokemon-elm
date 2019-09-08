module Main exposing (..)

import Bootstrap.CDN as CDN
import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav
import Debug exposing (log)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import Json.Decode as Decode exposing (Decoder, bool, dict, float, int, keyValuePairs, list, maybe, string)
import Json.Decode.Pipeline exposing (custom, hardcoded, required)
import List.Extra as List
import Platform.Cmd exposing (Cmd)
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>), Parser, int, map, oneOf, parse, s, top)



------------------------- MODEL


type alias Model =
    { navKey : Nav.Key
    , pokemons : List Pokemon
    , selectedPokemon : Maybe FullPokemon
    , currentPage : Route
    }


type Route
    = Pokedex
    | SinglePokemon Int
    | NotFound


type PokemonImage
    = Full
    | Sprite


type alias Pokemon =
    { id : Int
    , name : String
    , types : List String
    }


pokemonDecoder : Decoder Pokemon
pokemonDecoder =
    Decode.succeed Pokemon
        |> required "id" Decode.int
        |> required "name" string
        |> required "types" (list string)


type alias FullPokemon =
    { id : Int
    , name : String
    , height : Float
    , weight : Float
    , baseExperience : Int
    , position : Int
    , isDefault : Bool
    , captureRate : Int
    , baseHappiness : Int
    , genderRate : Int
    , hatchCounter : Int
    , types : List String
    , abilities : List PokemonAbilities
    , moves : List PokemonMoves
    , efficacies : Dict String Float
    }


fullPokemonDecoder : Decoder FullPokemon
fullPokemonDecoder =
    Decode.succeed FullPokemon
        |> required "id" Decode.int
        |> required "name" string
        |> required "height" float
        |> required "weight" float
        |> required "baseExperience" Decode.int
        |> required "position" Decode.int
        |> required "isDefault" bool
        |> required "captureRate" Decode.int
        |> required "baseHappiness" Decode.int
        |> required "genderRate" Decode.int
        |> required "hatchCounter" Decode.int
        |> required "types" (list string)
        |> required "abilities" (list pokemonAbilitiesDecoder)
        |> required "moves" (list pokemonMovesDecoder)
        |> required "efficacies" (dict float)


type alias PokemonAbilities =
    { effect : String
    , isHidden : Bool
    , name : String
    , shortEffect : String
    , slot : Int
    }


pokemonAbilitiesDecoder : Decoder PokemonAbilities
pokemonAbilitiesDecoder =
    Decode.succeed PokemonAbilities
        |> required "effect" string
        |> required "isHidden" bool
        |> required "name" string
        |> required "shortEffect" string
        |> required "slot" Decode.int


type alias PokemonMoves =
    { level : Maybe Int
    , order : Maybe Int
    , damageClass : String
    , name : String
    , power : Maybe Int
    , accuracy : Maybe Int
    , moveType : String
    , learnMethods : String
    , tmMachineNumber : Int
    }


pokemonMovesDecoder : Decoder PokemonMoves
pokemonMovesDecoder =
    Decode.succeed PokemonMoves
        |> required "level" (maybe Decode.int)
        |> required "order" (maybe Decode.int)
        |> required "damageClass" string
        |> required "name" string
        |> required "power" (maybe Decode.int)
        |> required "accuracy" (maybe Decode.int)
        |> required "moveType" string
        |> required "learnMethods" string
        |> required "tmMachineNumber" Decode.int


type alias PokemonStats =
    { attack : Int
    , defense : Int
    , spAttack : Int
    , spDefense : Int
    , speed : Int
    , total : Int
    }


pokemonStatsDecoder : Decoder PokemonStats
pokemonStatsDecoder =
    Decode.succeed PokemonStats
        |> required "attack" Decode.int
        |> required "defense" Decode.int
        |> required "spAttack" Decode.int
        |> required "spDefense" Decode.int
        |> required "speed" Decode.int
        |> required "total" Decode.int


pokemonTypes : Dict String String
pokemonTypes =
    Dict.fromList
        [ ( "normal", "#aa9" )
        , ( "fire", "#f42" )
        , ( "water", "#39f" )
        , ( "electric", "#fc3" )
        , ( "grass", "#7c5" )
        , ( "ice", "#6cf" )
        , ( "fighting", "#b54" )
        , ( "poison", "#a59" )
        , ( "ground", "#db5" )
        , ( "flying", "#89f" )
        , ( "psychic", "#f59" )
        , ( "bug", "#ab2" )
        , ( "rock", "#ba6" )
        , ( "ghost", "#66b" )
        , ( "dragon", "#76e" )
        , ( "dark", "#754" )
        , ( "steel", "#aab" )
        , ( "fairy", "#e9e" )
        ]


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ navKey =
    ( { navKey = navKey, pokemons = [], currentPage = Pokedex, selectedPokemon = Nothing }
    , Cmd.batch [ getPokedex ]
    )



--------------------- API CALLS


getPokedex : Cmd Msg
getPokedex =
    Http.get
        { url = "http://localhost:58803/api/Pokemons"
        , expect = Http.expectJson PokemonsReceived (Decode.list pokemonDecoder)
        }


getPokemon : Int -> Cmd Msg
getPokemon num =
    Http.get
        { url = "http://localhost:58803/api/Pokemons/" ++ String.fromInt num
        , expect = Http.expectJson PokemonsReceived (Decode.list pokemonDecoder)
        }



------------------------- UPDATE


type Msg
    = ChangeUrl Url
    | ClickLink UrlRequest
    | PokemonsReceived (Result Http.Error (List Pokemon))
    | FullPokemonReceived (Result Http.Error FullPokemon)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeUrl url ->
            case url |> Url.toString |> toRoute of
                Pokedex ->
                    ( { model | currentPage = Pokedex }, getPokedex )

                SinglePokemon num ->
                    ( { model | currentPage = SinglePokemon num }, getPokemon num )

                NotFound ->
                    ( { model | currentPage = NotFound }, Cmd.none )

        ClickLink urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model, Nav.pushUrl model.navKey <| Url.toString url )

                External url ->
                    ( model, Nav.load url )

        PokemonsReceived (Ok receivedPokemons) ->
            ( { model | pokemons = receivedPokemons }, Cmd.none )

        PokemonsReceived (Err _) ->
            ( model, Cmd.none )

        FullPokemonReceived (Ok receivedPokemon) ->
            ( { model | selectedPokemon = Maybe.Just receivedPokemon }, Cmd.none )

        FullPokemonReceived (Err _) ->
            ( model, Cmd.none )



------------------------ VIEW


view : Model -> Document Msg
view model =
    { title = "Pokemon"
    , body =
        [ div [ class "container" ]
            [ CDN.stylesheet
            , div [ class "row" ]
                [ div [ class "col" ]
                    [ navbar ]
                ]
            , case model.currentPage of
                Pokedex ->
                    pokedex model.pokemons

                SinglePokemon _ ->
                    case model.selectedPokemon of
                        Just poke ->
                            singlePokemon poke

                        Nothing ->
                            h2 [] [ text "Not found." ]

                NotFound ->
                    div [] [ text "Page not Found." ]
            ]
        ]
    }


pokedex : List Pokemon -> Html Msg
pokedex pokemons =
    div [ class "row" ]
        (List.map pokemon pokemons)


pokemon : Pokemon -> Html Msg
pokemon pokemonRecord =
    div [ class "col-2 text-center" ]
        [ pokemonImg Sprite pokemonRecord.name
        , button [ class "btn btn-link" ] [ text (capitalize True pokemonRecord.name) ]
        , div [ class "row justify-content-center" ]
            (List.map pokemonType pokemonRecord.types)
        ]


pokemonImg : PokemonImage -> String -> Html Msg
pokemonImg imgType pokemonName =
    let
        url =
            case imgType of
                Full ->
                    String.concat [ " https://img.pokemondb.net/artwork/", String.toLower pokemonName, ".jpg" ]

                Sprite ->
                    String.concat [ "https://img.pokemondb.net/sprites/omega-ruby-alpha-sapphire/dex/normal/", String.toLower pokemonName, ".png" ]
    in
    img [ class "img-fluid", style "margin" "0", src url ] []


pokemonType : String -> Html Msg
pokemonType name =
    div
        [ class "col-6"
        , style "color" (getColorFromType name)
        ]
        [ text name ]


singlePokemon : FullPokemon -> Html Msg
singlePokemon pokemonRecord =
    div []
        [ div
            [ class "row" ]
            [ div [ class "col-6" ]
                [ pokemonImg Full pokemonRecord.name
                ]
            , div [ class "col-6" ]
                [ h2 [ class "text-center" ] [ text pokemonRecord.name ]
                , div
                    [ class "row" ]
                    [ div
                        [ class "col-6" ]
                        [ table [ class "table" ]
                            [ tbody []
                                [ tr []
                                    [ td [ class "w-25 text-muted" ] [ text "Pokemon:" ]
                                    , td [] [ text pokemonRecord.name ]
                                    ]
                                , tr []
                                    [ td [ class "w-25 text-muted", style "vertical-align" "middle" ] [ text "Type:" ]
                                    , td [] (List.map pokemonType pokemonRecord.types)
                                    ]
                                , tr []
                                    [ td [ class "w-25 text-muted" ] [ text "Dex No.:" ]
                                    , td [] [ text (pokemonRecord.id |> String.fromInt) ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


navbar : Html Msg
navbar =
    nav [ class "navbar navbar-light bg-light justify-content-start" ]
        [ span [ class "navbar-brand mb-0 h1" ] [ text "Navbar" ]
        , ul [ class "navbar-nav" ]
            [ li [ class "nav-item" ]
                [ a [ class "nav-link", href "/pokedex" ] [ text "Pokédex" ]
                , a [ class "nav-link", href "/pokemon/2" ] [ text "Pokemon" ]
                ]
            ]
        ]



------------------- PROGRAM


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


route : Parser (Route -> a) a
route =
    oneOf
        [ map Pokedex top
        , map Pokedex (s "pokedex")
        , map SinglePokemon (s "pokemon" </> UrlParser.int)
        ]


toRoute : String -> Route
toRoute string =
    case Url.fromString string of
        Nothing ->
            NotFound

        Just url ->
            Maybe.withDefault NotFound (parse route url)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = ClickLink
        , onUrlChange = ChangeUrl
        }