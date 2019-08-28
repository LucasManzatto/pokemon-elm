module Main exposing (..)

import Bootstrap.CDN as CDN
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



---- MODEL ----


type alias Model =
    { pokemons : List String
    , newPokemon : String
    }


init : ( Model, Cmd Msg )
init =
    ( { pokemons = [ "Charmander", "Squirtle", "Bulbasaur" ], newPokemon = "" }, Cmd.none )



---- UPDATE ----


type Msg
    = AddPokemon String
    | UpdatePokemonInput String
    | DeletePokemon String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddPokemon newPokemon ->
            ( { model | pokemons = newPokemon :: model.pokemons }, Cmd.none )

        UpdatePokemonInput newInput ->
            ( { model | newPokemon = newInput }, Cmd.none )

        DeletePokemon poke ->
            ( { model | pokemons = List.remove poke model.pokemons }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [class "container"]
        [ CDN.stylesheet
        , div [ class "row" ]
            [ div [ class "col" ]
                [ navbar ]
            ]
        , div [ class "row" ]
            (pokemons model.pokemons)
        , button [ class "btn btn-primary mt-3", onClick (AddPokemon model.newPokemon) ] [ text "Add new Pokemon" ]
        , div [ class "row" ]
            [ div [ class "col-4" ]
                [ input [ class "form-control mt-3", onInput UpdatePokemonInput ] []
                ]
            ]
        ]


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


pokemons : List String -> List (Html Msg)
pokemons pokemonsList =
    List.map (\pk -> pokemon pk) pokemonsList


pokemon : String -> Html Msg
pokemon name =
    div [ class "col-2 text-center" ] [ text name, button [ class "btn btn-link", onClick (DeletePokemon name) ] [ text "X" ] ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }