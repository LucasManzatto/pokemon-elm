module Route exposing (..)

import Api exposing (..)
import Browser exposing (UrlRequest(..))
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (href)
import Types exposing (..)
import Url
import Url.Parser as UrlParser exposing ((</>), Parser, int, map, oneOf, parse, s, top)
import Utils exposing (..)



------------------------- MODEL


type Route
    = Pokedex
    | SinglePokemon Int
    | NotFound


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


toHref : Route -> Attribute msg
toHref targetRoute =
    href (routeToString targetRoute)


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                NotFound ->
                    [ "404" ]

                Pokedex ->
                    [ "pokedex" ]

                SinglePokemon id ->
                    [ "pokemon", String.fromInt id ]
    in
    "/" ++ String.join "/" pieces
