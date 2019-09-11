module Main exposing (..)

import Bootstrap.CDN as CDN
import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav
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
    , maxStats : Maybe PokemonStats
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ navKey =
    ( { navKey = navKey
      , pokemons = Loading
      , currentPage = Pokedex
      , selectedPokemon = NotAsked
      , selectedPokemonEvolutionChain = []
      , maxStats = Nothing
      }
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
                    ( { model | currentPage = Pokedex }, getPokedex )

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
                    ( { model | maxStats = Just maxStats }, Cmd.none )

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
                            singlePokemon poke model.selectedPokemonEvolutionChain

                        Failure _ ->
                            h2 [] [ text "HTTP Error" ]

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
                    (List.map (\elem -> pokemonType elem (firstType == elem) False) pokemonRecord.types)

            else
                div [ class "row justify-content-center no-gutters" ]
                    [ pokemonType firstType False False ]
    in
    div [ class "text-center", style "width" "12.5%" ]
        [ div []
            [ h2 [] []
            , pokemonImg Sprite pokemonRecord.name
            ]
        , div [] [ text formatedPokemonNumber ]
        , div []
            [ a [ style "padding" "0", class "btn btn-link font-weight-bold", href ("pokemon/" ++ String.fromInt pokemonRecord.id) ] [ text (capitalize pokemonRecord.name) ] ]
        , div []
            [ types ]
        ]


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
        div
            [ class "col-4 pokemon-type mr-3"
            , style "background-color" (getColorFromType name)
            ]
            [ text (capitalize name) ]

    else
        div
            [ class "col-4"
            , style "color" (getColorFromType name)
            ]
            [ text (capitalize name) ]


singlePokemon : FullPokemon -> List PokemonEvolutionChain -> Html Msg
singlePokemon pokemonRecord evolutionChain =
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
                            [ singlePokemonAllStats pokemonRecord.stats
                            ]
                        ]
                    ]
                , div [ class "col-5" ]
                    [ div [ class "row" ]
                        [ div [ class "col-12" ]
                            [ h2 [] [ text "Type Defenses" ] ]
                        , div [ class "col-12" ] []
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


singlePokemonAllStats : PokemonStats -> Html Msg
singlePokemonAllStats pokemonStats =
    table [ class "table table-sm" ]
        [ tbody []
            [ singlePokemonSingleStat HP pokemonStats.hp 300
            ]
        ]


singlePokemonSingleStat : Stats -> Int -> Int -> Html Msg
singlePokemonSingleStat statName statValue maxStatValue =
    let
        ( minValue, maxValue, isBold ) =
            case statName of
                HP ->
                    ( "10", "20", "" )

                Total ->
                    ( "Min", "Max", "font-weight-bold" )

                _ ->
                    ( "20", "20", "" )
    in
    tr []
        [ td [ class "text-muted", style "width" "16.66%" ] [ text "HP" ]
        , td [ class "text-muted", style "width" "8.33%" ] [ text (String.fromInt statValue) ]
        , td
            [ class "text-muted"
            , style "width" "58.33%"
            , style "vertical-align" "middle"
            ]
            [ div [ class "progress" ]
                [ div [ class "progressbar bg-danger", style "width" "50%" ]
                    []
                ]
            ]
        , td [ class "text-muted", style "width" "8.33%" ] [ text minValue ]
        , td [ class "text-muted", style "width" "8.33%" ] [ text maxValue ]
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
