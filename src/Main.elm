module Main exposing (..)

-- import Regex exposing (replace)

import Bootstrap.CDN as CDN
import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav
import Debug exposing (log)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import Json.Decode as Decode exposing (Decoder, bool, dict, float, int, keyValuePairs, list, maybe, nullable, string)
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, required)
import List.Extra as List
import Platform.Cmd exposing (Cmd)
import Regex exposing (..)
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>), Parser, int, map, oneOf, parse, s, top)



------------------------- MODEL


type alias Model =
    { navKey : Nav.Key
    , pokemons : WebData (List Pokemon)
    , selectedPokemon : WebData FullPokemon
    , currentPage : Route
    }


type RemoteData error value
    = NotAsked
    | Loading
    | Failure error
    | Success value


type alias WebData a =
    RemoteData Http.Error a


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
    , moveType : Maybe String
    , learnMethods : String
    , tmMachineNumber : Int
    }


pokemonMovesDecoder : Decoder PokemonMoves
pokemonMovesDecoder =
    Decode.succeed PokemonMoves
        |> required "level" (nullable Decode.int)
        |> required "order" (nullable Decode.int)
        |> optional "damageClass" string "Invalid Damage Class."
        |> optional "name" string "Invalid Name."
        |> required "power" (nullable Decode.int)
        |> required "accuracy" (nullable Decode.int)
        |> required "type" (nullable string)
        |> optional "learnMethods" string "Invalid Learn Methods."
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
    ( { navKey = navKey, pokemons = Loading, currentPage = Pokedex, selectedPokemon = NotAsked }
    , Cmd.batch [ getPokedex ]
    )



--------------------- API CALLS


getPokedex : Cmd Msg
getPokedex =
    Http.get
        { url = "http://localhost:58803/api/Pokemons"
        , expect = Http.expectJson (fromResult >> PokemonsReceived) (Decode.list pokemonDecoder)
        }


getPokemon : Int -> Cmd Msg
getPokemon num =
    Http.get
        { url = "http://localhost:58803/api/Pokemons/" ++ String.fromInt num
        , expect = Http.expectJson (fromResult >> FullPokemonReceived) fullPokemonDecoder
        }



------------------------- UPDATE


type Msg
    = ChangeUrl Url
    | ClickLink UrlRequest
    | PokemonsReceived (WebData (List Pokemon))
    | FullPokemonReceived (WebData FullPokemon)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeUrl url ->
            case url |> Url.toString |> toRoute of
                Pokedex ->
                    ( { model | currentPage = Pokedex }, getPokedex )

                SinglePokemon num ->
                    ( { model | currentPage = SinglePokemon num, selectedPokemon = Loading }, getPokemon num )

                NotFound ->
                    ( { model | currentPage = NotFound }, Cmd.none )

        ClickLink urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model, Nav.pushUrl model.navKey <| Url.toString url )

                External url ->
                    ( model, Nav.load url )

        PokemonsReceived response ->
            ( { model | pokemons = response }, Cmd.none )

        FullPokemonReceived response ->
            ( { model | selectedPokemon = response }, Cmd.none )



------------------------ VIEW


view : Model -> Document Msg
view model =
    { title = "Pokemon"
    , body =
        [ div [ class "container-fluid" ]
            [ CDN.stylesheet
            , div [ class "row" ]
                [ div [ class "col" ]
                    [ navbar ]
                ]
            , case model.currentPage of
                Pokedex ->
                    case model.pokemons of
                        NotAsked ->
                            text ""

                        Loading ->
                            h3 [] [ text "Loading..." ]

                        Success pokemons ->
                            pokedex pokemons

                        Failure _ ->
                            h2 [] [ text "HTTP Error" ]

                SinglePokemon _ ->
                    case model.selectedPokemon of
                        NotAsked ->
                            text ""

                        Loading ->
                            h3 [] [ text "Loading..." ]

                        Success poke ->
                            singlePokemon poke

                        Failure _ ->
                            h2 [] [ text "HTTP Error" ]

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
    let
        firstType =
            case List.head pokemonRecord.types of
                Just elem ->
                    elem

                Nothing ->
                    ""

        types =
            if List.length pokemonRecord.types == 2 then
                div [ class "row justify-content-center no-gutters" ]
                    (List.map (\elem -> pokemonType elem (firstType == elem)) pokemonRecord.types)

            else
                div [ class "row justify-content-center no-gutters" ]
                    [ pokemonType firstType False ]
    in
    div [ class "col-2 text-center" ]
        [ pokemonImg Sprite pokemonRecord.name
        , a [ class "btn btn-link", href ("pokemon/" ++ String.fromInt pokemonRecord.id) ] [ text (capitalize True pokemonRecord.name) ]
        , types
        ]


pokemonImg : PokemonImage -> String -> Html Msg
pokemonImg imgType pokemonName =
    case imgType of
        Full ->
            let
                url =
                    String.concat [ " https://img.pokemondb.net/artwork/", String.toLower pokemonName, ".jpg" ]
            in
            img [ class "img-fluid", style "margin" "0", style "width" "100%", style "heigth" "auto", src url ] []

        Sprite ->
            let
                url =
                    String.concat [ "https://img.pokemondb.net/sprites/omega-ruby-alpha-sapphire/dex/normal/", String.toLower pokemonName, ".png" ]
            in
            img [ class "img-fluid", style "margin" "0", src url ] []


pokemonType : String -> Bool -> Html Msg
pokemonType name renderDash =
    let
        dash =
            if renderDash then
                " - "

            else
                ""
    in
    div
        [ class "col-4"
        , style "color" (getColorFromType name)
        ]
        [ text name ]


singlePokemon : FullPokemon -> Html Msg
singlePokemon pokemonRecord =
    div []
        [ div
            [ class "row" ]
            [ div [ class "col-4" ]
                [ pokemonImg Full pokemonRecord.name
                ]
            , div [ class "col-8" ]
                [ h2 [ class "text-center" ] [ text (capitalize True pokemonRecord.name) ]
                , div
                    [ class "row" ]
                    [ div
                        [ class "col-6" ]
                        [ table [ class "table" ]
                            [ tbody []
                                [ tr []
                                    [ td [ class "w-25 text-muted" ] [ text "Pokemon:" ]
                                    , td [] [ text (capitalize True pokemonRecord.name) ]
                                    ]
                                , tr []
                                    [ td [ class "w-25 text-muted", style "vertical-align" "middle" ] [ text "Type:" ]
                                    , td [] (List.map (\elem -> pokemonType elem False) pokemonRecord.types)
                                    ]
                                , tr []
                                    [ td [ class "w-25 text-muted" ] [ text "Dex No.:" ]
                                    , td [] [ text (pokemonRecord.id |> String.fromInt) ]
                                    ]
                                , tr []
                                    [ td [ class "w-25 text-muted", style "vertical-align" "middle" ] [ text "Abilities.:" ]
                                    , td []
                                        (List.map pokemonAbility pokemonRecord.abilities)
                                    ]
                                , tr []
                                    [ td [ class "w-25 text-muted", style "vertical-align" "middle" ] [ text "Height:" ]
                                    , td []
                                        [ text (String.fromFloat pokemonRecord.height ++ " m") ]
                                    ]
                                , tr []
                                    [ td [ class "w-25 text-muted", style "vertical-align" "middle" ] [ text "Weight:" ]
                                    , td []
                                        [ text (String.fromFloat pokemonRecord.weight ++ " kg") ]
                                    ]
                                ]
                            ]
                        ]
                    , div [ class "col-6" ]
                        [ table [ class "table" ]
                            [ tbody []
                                [ tr []
                                    [ td [ class "text-muted", style "vertical-align" "middle", style "width" "35%" ] [ text "Base Exp.:" ]
                                    , td []
                                        [ text (String.fromInt pokemonRecord.baseExperience) ]
                                    ]
                                , tr []
                                    [ td [ class "text-muted", style "vertical-align" "middle", style "width" "35%" ] [ text "Capture Rate:" ]
                                    , td []
                                        [ text (String.fromInt pokemonRecord.captureRate) ]
                                    ]
                                , tr []
                                    [ td [ class "text-muted", style "vertical-align" "middle", style "width" "35%" ] [ text "Base Happiness:" ]
                                    , td []
                                        [ text (String.fromInt pokemonRecord.baseHappiness) ]
                                    ]
                                , tr []
                                    [ td [ class "text-muted", style "vertical-align" "middle", style "width" "35%" ] [ text "Gender:" ]
                                    , td []
                                        [ pokemonGender pokemonRecord.genderRate ]
                                    ]
                                , tr []
                                    [ td [ class "text-muted", style "vertical-align" "middle", style "width" "35%" ] [ text "Hatch Counter:" ]
                                    , td []
                                        [ text (String.fromInt (pokemonRecord.hatchCounter * 257) ++ " steps") ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


pokemonAbility : PokemonAbilities -> Html Msg
pokemonAbility ability =
    let
        abilityEffect =
            ability.shortEffect
                |> regexReplace "({.*?})" (\_ -> "")
                |> regexReplace "(\\[*\\]*)" (\_ -> "")

        abilityTextHtml =
            let
                abilityText =
                    capitalize True ability.name
            in
            if ability.isHidden then
                small [] [ text (abilityText ++ " (Hidden)") ]

            else
                text abilityText
    in
    div [ class "row" ]
        [ div [ class "col" ]
            [ div [ class "tooltip-custom" ]
                [ abilityTextHtml
                , span
                    [ class "tooltiptext" ]
                    [ text abilityEffect ]
                ]
            ]
        ]


pokemonGender : Int -> Html Msg
pokemonGender genderRate =
    if genderRate == -1 then
        text "Genderless"

    else
        let
            femalePercentage =
                (toFloat genderRate / 8) * 100

            malePercentage =
                100 - femalePercentage
        in
        div [ class "row justify-content-between" ]
            [ div [ class "col-6" ]
                [ span [ style "color" "#39f" ]
                    [ text (String.fromFloat malePercentage ++ "% Male") ]
                ]
            , div [ class "col-6" ]
                [ span
                    [ style "color" "#f59" ]
                    [ text (String.fromFloat femalePercentage ++ "% Female") ]
                ]
            ]


navbar : Html Msg
navbar =
    nav [ class "navbar navbar-light bg-light justify-content-start" ]
        [ span [ class "navbar-brand mb-0 h1" ] [ text "Navbar" ]
        , ul [ class "navbar-nav mr-auto" ]
            [ li [ class "nav-item" ]
                [ a [ class "nav-link", href "/pokedex" ] [ text "Pokédex" ]
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


regexReplace : String -> (Regex.Match -> String) -> String -> String
regexReplace userRegex replacer string =
    case Regex.fromString userRegex of
        Nothing ->
            string

        Just regex ->
            Regex.replace regex replacer string


fromResult : Result e a -> RemoteData e a
fromResult result =
    case result of
        Err e ->
            Failure e

        Ok x ->
            Success x


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
