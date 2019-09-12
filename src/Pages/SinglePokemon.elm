module Pages.SinglePokemon exposing (singlePokemon)

import Browser exposing (UrlRequest(..))
import Debug exposing (log)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Ionicon
import List.Extra as List
import Types exposing (..)
import Utils exposing (..)


type Msg
    = Msg


singlePokemon : FullPokemon -> List PokemonEvolutionChain -> PokemonStats -> Html Msg
singlePokemon pokemonRecord evolutionChain maxStats =
    div
        [ class "row justify-content-center" ]
        [ div [ class "col-4 text-center" ]
            [ pokemonImg Full pokemonRecord.name
            ]
        , div
            [ class "col-8" ]
            [ singlePokemonData pokemonRecord ]
        , div
            [ class "col-12" ]
            [ div [ class "row" ]
                [ div [ class "col-7" ]
                    [ div [ class "row" ]
                        [ div [ class "col-12" ]
                            [ h2 [] [ text "Stats" ] ]
                        , div [ class "col-12" ]
                            [ singlePokemonAllStats pokemonRecord.stats maxStats
                            ]
                        ]
                    ]
                , div [ class "col-5" ]
                    [ div [ class "row" ]
                        [ div [ class "col-12" ]
                            [ h2 [] [ text "Type Defenses" ] ]
                        , div [ class "col-12" ]
                            [ singlePokemonEfficacies pokemonRecord.efficacies
                            ]
                        ]
                    ]
                ]
            ]
        , div [ class "col-12" ]
            [ h2 [] [ text "Evolution Chain" ]
            ]
        , div
            [ class "col-10" ]
            [ singlePokemonEvolutionChain evolutionChain ]
        ]


singlePokemonEfficacies : Dict String Float -> Html Msg
singlePokemonEfficacies efficacies =
    let
        efficaciesList =
            Dict.toList efficacies

        halfEfficaciesList =
            List.length efficaciesList // 2

        groups =
            List.groupsOf halfEfficaciesList efficaciesList

        test =
            log "groups: " (List.head groups)

        html =
            case groups of
                [ firstRow, secondRow ] ->
                    div [ class "row" ]
                        [ div [ class "col-12" ]
                            [ div
                                [ class "row no-gutters" ]
                                (firstRow
                                    |> List.map
                                        (\( efficacy, value ) ->
                                            div [ style "width" "11.11%" ]
                                                [ div [] [ text efficacy ]
                                                , div [] [ text (String.fromFloat value) ]
                                                ]
                                        )
                                )
                            ]
                        , div [ class "col-12" ]
                            [ div
                                [ class "row no-gutters" ]
                                (secondRow
                                    |> List.map
                                        (\( efficacy, value ) ->
                                            div [ style "width" "11.11%" ]
                                                [ div [] [ text efficacy ]
                                                , div [] [ text (String.fromFloat value) ]
                                                ]
                                        )
                                )
                            ]
                        ]

                _ ->
                    div [] []
    in
    html


singlePokemonAllStats : PokemonStats -> PokemonStats -> Html Msg
singlePokemonAllStats pokemonStats maxStats =
    table [ class "table table-sm" ]
        [ tbody []
            [ singlePokemonSingleStat "HP" pokemonStats.hp maxStats.hp
            , singlePokemonSingleStat "Attack" pokemonStats.attack maxStats.attack
            , singlePokemonSingleStat "Defense" pokemonStats.defense maxStats.defense
            , singlePokemonSingleStat "Sp. Attack" pokemonStats.spAttack maxStats.spAttack
            , singlePokemonSingleStat "Sp. Defense" pokemonStats.spDefense maxStats.spDefense
            , singlePokemonSingleStat "Speed" pokemonStats.speed maxStats.speed
            , singlePokemonSingleStat "Total" pokemonStats.total 0
            ]
        ]


singlePokemonSingleStat : String -> Int -> Int -> Html Msg
singlePokemonSingleStat statName statValue maxStatValue =
    let
        statsPercentageNumeric =
            (toFloat statValue / toFloat maxStatValue) * 100

        statsPercentageString =
            String.fromFloat statsPercentageNumeric ++ "%"

        progressBarColor =
            if statsPercentageNumeric < 25 then
                "bg-danger"

            else if statsPercentageNumeric < 50 then
                "bg-warning"

            else if statsPercentageNumeric < 75 then
                "bg-success"

            else
                "bg-info"

        bar =
            div [ class "progress" ]
                [ div [ class ("progressbar " ++ progressBarColor), style "width" statsPercentageString ]
                    []
                ]

        { minValue, maxValue, isBold, progressBar } =
            case statName of
                "HP" ->
                    let
                        min =
                            String.fromInt (calculateHpStat 0 0 statValue)

                        max =
                            String.fromInt (calculateHpStat 31 255 statValue)
                    in
                    { minValue = min, maxValue = max, isBold = "", progressBar = bar }

                "Total" ->
                    { minValue = "Min", maxValue = "Max", isBold = "font-weight-bold", progressBar = div [] [] }

                _ ->
                    let
                        min =
                            String.fromInt (calculateOtherStat 0 0 0.9 statValue)

                        max =
                            String.fromInt (calculateOtherStat 31 255 1.1 statValue)
                    in
                    { minValue = min, maxValue = max, isBold = "", progressBar = bar }
    in
    tr []
        [ td [ class ("text-muted " ++ isBold), style "width" "16.66%" ] [ text statName ]
        , td [ class ("text-muted " ++ isBold), style "width" "8.33%" ] [ text (String.fromInt statValue) ]
        , td
            [ class "text-muted"
            , style "width" "58.33%"
            , style "vertical-align" "middle"
            ]
            [ progressBar
            ]
        , td [ class ("text-muted " ++ isBold), style "width" "8.33%" ] [ text minValue ]
        , td [ class ("text-muted " ++ isBold), style "width" "8.33%" ] [ text maxValue ]
        ]


singlePokemonEvolutionChain : List PokemonEvolutionChain -> Html Msg
singlePokemonEvolutionChain evolutionChain =
    let
        evolutionChainHtml =
            case List.uncons evolutionChain of
                Just ( firstFromChain, chainExceptFirst ) ->
                    div [ class "row text-center justify-content-center" ]
                        (List.append
                            [ div
                                [ class "col-2" ]
                                [ div []
                                    [ pokemonImg Sprite firstFromChain.name ]
                                , div []
                                    [ text (capitalize firstFromChain.name) ]
                                ]
                            ]
                            (List.map
                                (\pokemon ->
                                    div [ class "col-4" ]
                                        [ div [ class "row text-center" ]
                                            [ div [ class "col-6 d-flex align-items-center justify-content-center" ]
                                                [ div [ class "row" ]
                                                    [ div [ class "col-12" ] [ Ionicon.arrowRightC 32 (RGBA 0.14 0.06 0 0.51) ]
                                                    , div [ class "col-12" ] [ text pokemon.evolutionCondition ]
                                                    ]
                                                ]
                                            , div [ class "col-6" ]
                                                [ div []
                                                    [ pokemonImg Sprite pokemon.name ]
                                                , div []
                                                    [ text (capitalize pokemon.name) ]
                                                ]
                                            ]
                                        ]
                                )
                                chainExceptFirst
                            )
                        )

                Nothing ->
                    div [] []
    in
    evolutionChainHtml


singlePokemonData : FullPokemon -> Html Msg
singlePokemonData pokemonRecord =
    div []
        [ h2 [ class "text-center" ] [ text (capitalize pokemonRecord.name) ]
        , div
            [ class "row" ]
            [ div
                [ class "col-6" ]
                [ table [ class "table" ]
                    [ tbody []
                        [ tr []
                            [ td [ class "w-25 text-muted" ] [ text "Pokemon:" ]
                            , td [] [ text (capitalize pokemonRecord.name) ]
                            ]
                        , tr []
                            [ td [ class "w-25 text-muted", style "vertical-align" "middle" ] [ text "Type:" ]
                            , td [] (List.map (\elem -> pokemonType elem False True) pokemonRecord.types)
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


pokemonType : String -> Bool -> Bool -> Html Msg
pokemonType name renderDash isButtonStyle =
    let
        dash =
            if renderDash then
                " - "

            else
                ""
    in
    if isButtonStyle then
        button
            [ class "pokemon-type"
            , style "background-color" (getColorFromType name)
            ]
            [ text (capitalize name) ]

    else
        button
            [ class "btn btn-link"
            , style "color" (getColorFromType name)
            ]
            [ text (capitalize name) ]


pokemonImg : PokemonImage -> String -> Html Msg
pokemonImg imgType pokemonName =
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
                    String.concat [ "https://img.pokemondb.net/sprites/omega-ruby-alpha-sapphire/dex/normal/", String.toLower pokemonName, ".png" ]
            in
            img
                [ class "img-fluid"
                , style "margin" "0"
                , style "position" "relative"
                , style "top" "15px"
                , style "z-index" "-10"
                , src url
                ]
                []


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
                    capitalize ability.name
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
