module Pages.Shared exposing (..)

import Browser exposing (UrlRequest(..))
import Debug exposing (log)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Route exposing (..)
import Types exposing (..)
import Utils exposing (..)


pokemonType : Bool -> Bool -> String -> Html msg
pokemonType isAbbreviated isButtonStyle name =
    let
        formatedName =
            (if isAbbreviated then
                String.left 3 name

             else
                name
            )
                |> capitalize
    in
    if isButtonStyle then
        button
            [ class "pokemon-type"
            , style "background-color" (getColorFromType name)
            , style "width" "95%"
            , style "min-width" "60px"
            , style "max-width" "92px"
            , style "margin" "0"
            ]
            [ text formatedName ]

    else
        a
            [ class "ml-1 mr-1"
            , style "color" (getColorFromType name)
            , style "padding" "0"
            ]
            [ text formatedName ]


pokemonTypeFull : String -> Html msg
pokemonTypeFull =
    pokemonType False True


pokemonTypeLink : String -> Html msg
pokemonTypeLink =
    pokemonType False False


pokemonTypeAbbreviated : String -> Html msg
pokemonTypeAbbreviated =
    pokemonType True True


pokemonCard : Pokemon -> List (Html msg)
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
        [ a
            [ style "padding" "0"
            , class "btn btn-link font-weight-bold"
            , Route.toHref (Route.SinglePokemon pokemonRecord.id)
            ]
            [ text (capitalize pokemonRecord.name) ]
        ]
    , div []
        [ types ]
    ]


pokemonImg : PokemonImage -> String -> String -> Html msg
pokemonImg imgType pokemonName generation =
    case imgType of
        Full ->
            let
                url =
                    String.concat [ " https://img.pokemondb.net/artwork/", String.toLower pokemonName, ".jpg" ]
            in
            img [ class "img-fluid", src url ] []

        Sprite ->
            let
                url =
                    if generation == "generation-vii" then
                        String.concat [ "https://img.pokemondb.net/sprites/ultra-sun-ultra-moon/small/", String.toLower pokemonName, ".jpg" ]

                    else
                        String.concat [ "https://img.pokemondb.net/sprites/omega-ruby-alpha-sapphire/dex/normal/", String.toLower pokemonName, ".png" ]
            in
            img
                [ class "img-fluid"
                , style "margin" "0"
                , style "position" "relative"
                , style "top" "10px"
                , style "z-index" "-10"
                , src url
                ]
                []


pokemonImgFull : String -> Html msg
pokemonImgFull pokemonName =
    pokemonImg Full pokemonName ""


pokemonImgSprite : String -> String -> Html msg
pokemonImgSprite pokemonName generation =
    pokemonImg Sprite pokemonName generation
