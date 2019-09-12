module Utils exposing (..)

import Browser exposing (UrlRequest(..))
import Dict exposing (Dict)
import Http exposing (get)
import Regex exposing (fromString, replace)
import Types exposing (..)
import Url
import Url.Parser as UrlParser exposing ((</>), Parser, int, map, oneOf, parse, s, top)


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


efficacyColors : Dict Float String
efficacyColors =
    Dict.fromList
        [ ( 0, "black" )
        , ( 0.25, "darkred" )
        , ( 0.5, "firebrick" )
        , ( 1, "none" )
        , ( 2, "forestgreen" )
        , ( 4, "darkgreen" )
        ]


getColorFromType : String -> String
getColorFromType typeName =
    case Dict.get typeName pokemonTypes of
        Just color ->
            color

        Nothing ->
            "#000000"


getEfficacyColor : Float -> String
getEfficacyColor efficacy =
    case Dict.get efficacy efficacyColors of
        Just color ->
            color

        Nothing ->
            "white"


capitalize : String -> String
capitalize str =
    case String.uncons str of
        Nothing ->
            str

        Just ( firstLetter, rest ) ->
            let
                newFirstLetter =
                    Char.toUpper firstLetter
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


calculateHpStat : Int -> Int -> Int -> Int
calculateHpStat iv ev baseStat =
    (2 * baseStat + iv + (ev // 4)) + 110


calculateOtherStat : Int -> Int -> Float -> Int -> Int
calculateOtherStat iv ev natureMultiplier baseStat =
    round (toFloat (((2 * baseStat) + iv + (ev // 4)) + 5) * natureMultiplier)
