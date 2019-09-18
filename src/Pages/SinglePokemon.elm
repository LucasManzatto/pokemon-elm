module Pages.SinglePokemon exposing (..)

import Api as Api exposing (..)
import Browser exposing (Document, UrlRequest(..))
import Debug exposing (log)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (get)
import Ionicon
import Json.Decode as Decode exposing (list)
import List.Extra as List
import Pages.PokemonMoves as PokemonMoves exposing (..)
import Pages.Shared as Shared exposing (..)
import Platform.Cmd exposing (Cmd)
import Regex exposing (replace)
import Types exposing (..)
import Url exposing (Url)
import Utils exposing (..)


type alias Model =
    { pokemon : WebData FullPokemon
    , maxStats : WebData PokemonStats
    , evolutionChain : WebData (List PokemonEvolutionChain)
    }


initModel : Model
initModel =
    { pokemon = NotAsked
    , maxStats = NotAsked
    , evolutionChain = NotAsked
    }


getPokemon : Int -> Cmd Msg
getPokemon pokemonId =
    Http.get
        { url = apiUrl ++ "/Pokemons/" ++ String.fromInt pokemonId
        , expect = Http.expectJson (fromResult >> FullPokemonReceived) fullPokemonDecoder
        }


getMaxStats : Cmd Msg
getMaxStats =
    Http.get
        { url = apiUrl ++ "/Pokemons/maxStats"
        , expect =
            Http.expectJson (fromResult >> MaxStatsReceived) pokemonStatsDecoder
        }


getPokemonEvolutionChain : Int -> Cmd Msg
getPokemonEvolutionChain pokemonId =
    Http.get
        { url = apiUrl ++ "/Pokemons/" ++ String.fromInt pokemonId ++ "/evolutionChain"
        , expect = Http.expectJson (fromResult >> PokemonEvolutionChainReceived) (Decode.list pokemonEvolutionChainDecoder)
        }


init : () -> Int -> ( Model, Cmd Msg )
init _ pokemonId =
    ( initModel
    , Cmd.batch [ getPokemon pokemonId, getMaxStats, getPokemonEvolutionChain pokemonId ]
    )



------------------------------ UPDATE -------------------------


type Msg
    = FullPokemonReceived (WebData FullPokemon)
    | MaxStatsReceived (WebData PokemonStats)
    | PokemonEvolutionChainReceived (WebData (List PokemonEvolutionChain))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FullPokemonReceived response ->
            ( { model | pokemon = response }, Cmd.none )

        MaxStatsReceived response ->
            ( { model | maxStats = response }, Cmd.none )

        PokemonEvolutionChainReceived response ->
            ( { model | evolutionChain = response }, Cmd.none )



--------------------------- VIEW --------------------


view : Model -> Html msg
view model =
    case model.pokemon of
        NotAsked ->
            text ""

        Loading ->
            text "Loading"

        Success pokemon ->
            viewPokemon pokemon model.maxStats model.evolutionChain

        Failure _ ->
            text "Error"


viewPokemon : FullPokemon -> WebData PokemonStats -> WebData (List PokemonEvolutionChain) -> Html msg
viewPokemon pokemon maxStats evolutionChain =
    let
        viewMaxStats =
            case maxStats of
                NotAsked ->
                    text ""

                Loading ->
                    text "Loading..."

                Success maxStatsRes ->
                    allStats pokemon.stats maxStatsRes

                Failure _ ->
                    text "Error getting max stats."

        viewEvoChain =
            case evolutionChain of
                NotAsked ->
                    text ""

                Loading ->
                    text "Loading..."

                Success evolutionChainRes ->
                    viewEvolutionChain evolutionChainRes

                Failure _ ->
                    text "Error getting evolution chain."
    in
    div
        [ class "row justify-content-center" ]
        [ div [ class "col-4 text-center" ]
            [ pokemonImgFull pokemon.name
            ]
        , div
            [ class "col-8" ]
            [ singlePokemonData pokemon ]
        , div
            [ class "col-12" ]
            [ div [ class "row" ]
                [ div [ class "col-8" ]
                    [ div [ class "row" ]
                        [ div [ class "col-12" ]
                            [ h2 [] [ text "Stats" ] ]
                        , div [ class "col-12" ]
                            [ viewMaxStats
                            ]
                        ]
                    ]
                , div [ class "col-4" ]
                    [ div [ class "row" ]
                        [ div [ class "col-12" ]
                            [ h2 [] [ text "Type Defenses" ] ]
                        , div [ class "col-12" ]
                            [ viewEfficacies pokemon.efficacies
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
            [ viewEvoChain ]
        , div [ class "col-12" ]
            [ h2 [] [ text "Moves" ] ]
        , div [ class "col-12" ] [ PokemonMoves.view pokemon.moves ]
        ]


allStats : PokemonStats -> PokemonStats -> Html msg
allStats pokemonStats maxStats =
    table [ class "table table-sm" ]
        [ tbody []
            [ singleStat "HP" pokemonStats.hp maxStats.hp
            , singleStat "Attack" pokemonStats.attack maxStats.attack
            , singleStat "Defense" pokemonStats.defense maxStats.defense
            , singleStat "Sp. Attack" pokemonStats.spAttack maxStats.spAttack
            , singleStat "Sp. Defense" pokemonStats.spDefense maxStats.spDefense
            , singleStat "Speed" pokemonStats.speed maxStats.speed
            , singleStat "Total" pokemonStats.total 0
            ]
        ]


viewEfficacies : Dict String Float -> Html msg
viewEfficacies efficacies =
    let
        efficaciesList =
            Dict.toList efficacies

        halfEfficaciesList =
            List.length efficaciesList // 2

        groups =
            List.groupsOf halfEfficaciesList efficaciesList

        html =
            case groups of
                [ firstRow, secondRow ] ->
                    div [ class "row" ]
                        [ efficacyRow firstRow
                        , efficacyRow secondRow
                        ]

                _ ->
                    div [] []
    in
    html


efficacyRow : List ( String, Float ) -> Html msg
efficacyRow row =
    div [ class "col-12" ]
        [ div
            [ class "row no-gutters" ]
            (row
                |> List.map
                    (\( pokemonTypeName, multiplier ) ->
                        div [ style "width" "11.11%" ]
                            [ div [ class "mb-1" ] [ pokemonTypeAbbreviated pokemonTypeName ]
                            , efficacy multiplier
                            ]
                    )
            )
        ]


efficacy : Float -> Html msg
efficacy multiplier =
    let
        isNeutral =
            multiplier == 1

        html =
            if isNeutral then
                div [] []

            else
                button
                    [ class "pokemon-type"
                    , style "width" "95%"
                    , style "background-color" (getEfficacyColor multiplier)
                    , disabled True
                    ]
                    [ text (String.fromFloat multiplier) ]
    in
    html


singleStat : String -> Int -> Int -> Html msg
singleStat statName statValue maxStatValue =
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


viewEvolutionChain : List PokemonEvolutionChain -> Html msg
viewEvolutionChain evoChain =
    let
        evolutionChainHtml =
            case List.uncons evoChain of
                Just ( firstPokemonFromChain, chainExceptFirst ) ->
                    div [ class "row text-center justify-content-center" ]
                        (List.append
                            [ div
                                [ class "col-2" ]
                                [ div []
                                    (pokemonCard { id = firstPokemonFromChain.id, name = firstPokemonFromChain.name, generation = firstPokemonFromChain.generation, types = [] })
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
                                                    (pokemonCard { id = pokemon.id, name = pokemon.name, generation = pokemon.generation, types = [] })
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


singlePokemonData : FullPokemon -> Html msg
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
                            , td []
                                [ div [ class "row" ]
                                    (pokemonRecord.types
                                        |> List.map
                                            (\elem ->
                                                div [ class "col-4 no-gutters" ] [ pokemonTypeFull elem ]
                                            )
                                    )
                                ]
                            ]
                        , tr []
                            [ td [ class "w-25 text-muted" ] [ text "Dex No.:" ]
                            , td [] [ text (pokemonRecord.id |> String.fromInt) ]
                            ]
                        , tr []
                            [ td [ class "w-25 text-muted", style "vertical-align" "middle" ] [ text "Abilities.:" ]
                            , td []
                                (pokemonRecord.abilities
                                    |> List.map
                                        (\elem ->
                                            div [ class "row" ]
                                                [ div [ class "col" ]
                                                    [ pokemonAbility elem ]
                                                ]
                                        )
                                )
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


pokemonAbility : PokemonAbilities -> Html msg
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
    div [ class "tooltip-custom" ]
        [ abilityTextHtml
        , span
            [ class "tooltiptext" ]
            [ text ability.shortEffect ]
        ]


pokemonGender : Int -> Html msg
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
