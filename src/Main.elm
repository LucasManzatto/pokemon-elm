module Main exposing (..)

import Bootstrap.CDN as CDN
import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav
import Debug exposing (log)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (get)
import Ionicon
import Json.Decode as Decode exposing (list)
import List.Extra as List
import Platform.Cmd exposing (Cmd)
import Regex exposing (replace)
import Types exposing (..)
import Url exposing (Url)
import Utils exposing (..)



------------------------- MODEL


type alias Model =
    { navKey : Nav.Key
    , pokemons : WebData (List Pokemon)
    , selectedPokemon : WebData FullPokemon
    , selectedPokemonEvolutionChain : List PokemonEvolutionChain
    , currentPage : Route
    , maxStats : PokemonStats
    }


maxStatsInit : PokemonStats
maxStatsInit =
    { hp = 0, attack = 0, defense = 0, spAttack = 0, spDefense = 0, speed = 0, total = 0 }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    ( { navKey = navKey
      , pokemons = NotAsked
      , currentPage = url |> Url.toString |> toRoute
      , selectedPokemon = NotAsked
      , selectedPokemonEvolutionChain = []
      , maxStats = maxStatsInit
      }
    , Cmd.batch [ getMaxStats, Nav.pushUrl navKey <| Url.toString url ]
    )



--------------------- API CALLS


getPokedex : Cmd Msg
getPokedex =
    Http.get
        { url = "http://localhost:58803/api/Pokemons"
        , expect = Http.expectJson (fromResult >> PokemonsReceived) (Decode.list pokemonDecoder)
        }


getPokemon : Int -> Cmd Msg
getPokemon pokemonId =
    Http.get
        { url = "http://localhost:58803/api/Pokemons/" ++ String.fromInt pokemonId
        , expect = Http.expectJson (fromResult >> FullPokemonReceived) fullPokemonDecoder
        }


getPokemonEvolutionChain : Int -> Cmd Msg
getPokemonEvolutionChain pokemonId =
    Http.get
        { url = "http://localhost:58803/api/Pokemons/" ++ String.fromInt pokemonId ++ "/evolutionChain"
        , expect = Http.expectJson (fromResult >> PokemonEvolutionChainReceived) (Decode.list pokemonEvolutionChainDecoder)
        }


getMaxStats : Cmd Msg
getMaxStats =
    Http.get
        { url = "http://localhost:58803/api/Pokemons/maxStats"
        , expect =
            Http.expectJson (fromResult >> MaxStatsReceived) pokemonStatsDecoder
        }



-- api/Pokemons/maxStats
------------------------- UPDATE


type Msg
    = ChangeUrl Url
    | ClickLink UrlRequest
    | PokemonsReceived (WebData (List Pokemon))
    | FullPokemonReceived (WebData FullPokemon)
    | PokemonEvolutionChainReceived (WebData (List PokemonEvolutionChain))
    | MaxStatsReceived (WebData PokemonStats)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeUrl url ->
            case url |> Url.toString |> toRoute of
                Pokedex ->
                    let
                        loadPokedex =
                            case model.pokemons of
                                NotAsked ->
                                    getPokedex

                                _ ->
                                    Cmd.none
                    in
                    ( { model | currentPage = Pokedex }, loadPokedex )

                SinglePokemon pokemonId ->
                    ( { model | currentPage = SinglePokemon pokemonId, selectedPokemon = Loading }
                    , Cmd.batch [ getPokemon pokemonId, getPokemonEvolutionChain pokemonId ]
                    )

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

        PokemonEvolutionChainReceived response ->
            case response of
                Success evolutionChain ->
                    ( { model | selectedPokemonEvolutionChain = evolutionChain }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        MaxStatsReceived response ->
            case response of
                Success maxStats ->
                    ( { model | maxStats = maxStats }, Cmd.none )

                _ ->
                    ( model, Cmd.none )



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
                            singlePokemon poke model.selectedPokemonEvolutionChain model.maxStats

                        Failure _ ->
                            h2 [] [ text "Pokémon not found." ]

                NotFound ->
                    div [] [ text "Page not Found." ]
            ]
        ]
    }


pokedex : List Pokemon -> Html Msg
pokedex pokemons =
    let
        pokemonsGroupedByGeneration =
            List.gatherEqualsBy .generation pokemons

        pokedexGens =
            List.map pokedexGeneration pokemonsGroupedByGeneration
    in
    div [ class "row" ]
        pokedexGens


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

                [] ->
                    ""

                [ _ ] ->
                    ""

                _ :: _ :: _ ->
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
            [ div [ class "col" ] [ text formatedGen ]
            ]
        , div [ class "row" ]
            (List.map pokedexPokemon (firstPokemonFromGen :: genList))
        ]


pokedexPokemon : Pokemon -> Html Msg
pokedexPokemon pokemonRecord =
    let
        firstType =
            case List.head pokemonRecord.types of
                Just elem ->
                    elem

                Nothing ->
                    ""

        formatedPokemonNumber =
            "#" ++ String.padLeft 3 '0' (String.fromInt pokemonRecord.id)

        types =
            if List.length pokemonRecord.types == 2 then
                div [ class "row justify-content-center no-gutters" ]
                    (List.map (\elem -> pokemonTypeLink (firstType == elem) elem) pokemonRecord.types)

            else
                div [ class "row justify-content-center no-gutters" ]
                    [ pokemonTypeLink False firstType ]
    in
    div [ class "text-center", style "width" "12.5%" ]
        [ div []
            [ h2 [] []
            , pokemonImgSprite pokemonRecord.name pokemonRecord.generation
            ]
        , div [] [ text formatedPokemonNumber ]
        , div []
            [ a [ style "padding" "0", class "btn btn-link font-weight-bold", href ("pokemon/" ++ String.fromInt pokemonRecord.id) ] [ text (capitalize pokemonRecord.name) ] ]
        , div []
            [ types ]
        ]


pokemonImg : PokemonImage -> String -> String -> Html Msg
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


pokemonImgFull : String -> Html Msg
pokemonImgFull pokemonName =
    pokemonImg Full pokemonName ""


pokemonImgSprite : String -> String -> Html Msg
pokemonImgSprite pokemonName generation =
    pokemonImg Sprite pokemonName generation


pokemonType : Bool -> Bool -> Bool -> String -> Html Msg
pokemonType isAbbreviated isButtonStyle renderDash name =
    let
        formatedName =
            (if isAbbreviated then
                String.left 3 name

             else
                name
            )
                |> capitalize

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
            , style "width" "95%"
            ]
            [ text formatedName ]

    else
        button
            [ class "btn btn-link"
            , style "color" (getColorFromType name)
            ]
            [ text formatedName ]


pokemonTypeFull : String -> Html Msg
pokemonTypeFull =
    pokemonType False True False


pokemonTypeLink : Bool -> String -> Html Msg
pokemonTypeLink =
    pokemonType False False


pokemonTypeAbbreviated : String -> Html Msg
pokemonTypeAbbreviated =
    pokemonType True True False


singlePokemon : FullPokemon -> List PokemonEvolutionChain -> PokemonStats -> Html Msg
singlePokemon pokemonRecord evolutionChain maxStats =
    div
        [ class "row justify-content-center" ]
        [ div [ class "col-4 text-center" ]
            [ pokemonImgFull pokemonRecord.name
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
        , div [ class "col-12" ]
            [ h2 [] [ text "Moves" ] ]
        , div [ class "col-12" ] [ singlePokemonMoves pokemonRecord.moves ]
        ]


singlePokemonMoves : List PokemonMoves -> Html Msg
singlePokemonMoves pokemonMoves =
    let
        groupedMoves =
            List.gatherEqualsBy .learnMethods pokemonMoves

        groupedMovesOrderList =
            [ "level-up", "egg", "machine", "tutor" ]

        orderedGroupMoves =
            groupedMovesOrderList
                |> List.map
                    (\key ->
                        case List.find (\( firstMove, _ ) -> firstMove.learnMethods == key) groupedMoves of
                            Just ( firstMove, rest ) ->
                                div []
                                    [ h2 []
                                        [ text
                                            (case firstMove.learnMethods of
                                                "level-up" ->
                                                    "Leveling"

                                                "machine" ->
                                                    "TM"

                                                "egg" ->
                                                    "Egg"

                                                "tutor" ->
                                                    "Tutor"

                                                _ ->
                                                    "Undefined Group"
                                            )
                                        ]
                                    , table [ class "table table-hover table-sm text-center" ]
                                        [ thead []
                                            [ tr []
                                                [ if firstMove.learnMethods == "level-up" then
                                                    th [ scope "col", class "text-right" ]
                                                        [ text "Level" ]

                                                  else if firstMove.learnMethods == "machine" then
                                                    th [ scope "col", class "text-right" ]
                                                        [ text "TM" ]

                                                  else
                                                    text ""
                                                , th [ scope "col", class "text-left" ]
                                                    [ text "Move" ]
                                                , th [ scope "col" ]
                                                    [ text "Type" ]
                                                , th [ scope "col" ]
                                                    [ text "Category" ]
                                                , th [ scope "col" ]
                                                    [ text "Power" ]
                                                , th [ scope "col" ]
                                                    [ text "Accuracy" ]
                                                ]
                                            ]
                                        , tbody [ class "font-weight-normal" ]
                                            (List.map
                                                (\moveRow ->
                                                    tr []
                                                        [ if firstMove.learnMethods == "level-up" then
                                                            case moveRow.level of
                                                                Just level ->
                                                                    th [ class "text-right" ] [ text (String.fromInt level) ]

                                                                Nothing ->
                                                                    th [] [ text "-" ]

                                                          else if firstMove.learnMethods == "machine" then
                                                            th [ class "text-right" ] [ text (String.fromInt moveRow.tmMachineNumber) ]

                                                          else
                                                            text ""
                                                        , th [ class "text-left" ]
                                                            [ text
                                                                (moveRow.name
                                                                    |> String.replace "-" " "
                                                                    |> String.split " "
                                                                    |> List.map capitalize
                                                                    |> String.join " "
                                                                )
                                                            ]
                                                        , th []
                                                            [ case moveRow.moveType of
                                                                Just moveType ->
                                                                    pokemonTypeFull moveType

                                                                Nothing ->
                                                                    text "-"
                                                            ]
                                                        , th []
                                                            [ img
                                                                [ src ("https://img.pokemondb.net/images/icons/" ++ moveRow.damageClass ++ ".png")
                                                                , style "margin" "0"
                                                                , style "padding" "0"
                                                                ]
                                                                []
                                                            ]
                                                        , th []
                                                            [ case moveRow.power of
                                                                Just power ->
                                                                    text (String.fromInt power)

                                                                Nothing ->
                                                                    text "-"
                                                            ]
                                                        , th []
                                                            [ case moveRow.accuracy of
                                                                Just accuracy ->
                                                                    text (String.fromInt accuracy)

                                                                Nothing ->
                                                                    text "-"
                                                            ]
                                                        ]
                                                )
                                                (firstMove :: rest)
                                            )
                                        ]
                                    ]

                            Nothing ->
                                div [] []
                    )
                |> List.groupsOf 2

        html =
            div [ class "row" ]
                (List.map
                    (\col ->
                        div [ class "col-6" ]
                            [ div [ class "row" ]
                                [ div [ class "col-12" ]
                                    col
                                ]
                            ]
                    )
                    orderedGroupMoves
                )
    in
    html


singlePokemonEfficacies : Dict String Float -> Html Msg
singlePokemonEfficacies efficacies =
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
                        [ singlePokemonEfficacyRow firstRow
                        , singlePokemonEfficacyRow secondRow
                        ]

                _ ->
                    div [] []
    in
    html


singlePokemonEfficacyRow : List ( String, Float ) -> Html Msg
singlePokemonEfficacyRow efficacyRow =
    div [ class "col-12" ]
        [ div
            [ class "row no-gutters" ]
            (efficacyRow
                |> List.map
                    (\( pokemonTypeName, multiplier ) ->
                        div [ style "width" "11.11%" ]
                            [ div [] [ pokemonTypeAbbreviated pokemonTypeName ]
                            , efficacy multiplier
                            ]
                    )
            )
        ]


efficacy : Float -> Html Msg
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
                                    [ pokemonImgSprite firstFromChain.name firstFromChain.generation ]
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
                                                    [ pokemonImgSprite pokemon.name pokemon.generation ]
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
    div [ class "tooltip-custom" ]
        [ abilityTextHtml
        , span
            [ class "tooltiptext" ]
            [ text ability.shortEffect ]
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
