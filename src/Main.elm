module Main exposing (..)

import Bootstrap.CDN as CDN
import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav
import Debug exposing (log)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Pages.Pokedex as Pokedex exposing (..)
import Pages.SinglePokemon as SinglePokemon exposing (..)
import Platform.Cmd exposing (Cmd)
import Route exposing (Route)
import Types exposing (..)
import Url exposing (Url)
import Utils exposing (..)


type alias Model =
    { currentPage : CurrentPage
    , navKey : Nav.Key
    }


type CurrentPage
    = Pokedex Pokedex.Model
    | SinglePokemon SinglePokemon.Model


pokedexModel : Pokedex.Model
pokedexModel =
    { pokemons = []
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    changeRouteTo (url |> Url.toString |> Route.toRoute) { currentPage = Pokedex pokedexModel, navKey = navKey }



-------------------------- UPDATE --------------------


type Msg
    = ChangedUrl Url
    | ClickedLink UrlRequest
    | GotPokedexMsg Pokedex.Msg
    | GotPokemonMsg SinglePokemon.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPokedexMsg subMsg ->
            case model.currentPage of
                Pokedex pokedex ->
                    Pokedex.update subMsg pokedex
                        |> updateWith Pokedex GotPokedexMsg model

                _ ->
                    ( model, Cmd.none )

        GotPokemonMsg subMsg ->
            case model.currentPage of
                SinglePokemon singlePokemon ->
                    SinglePokemon.update subMsg singlePokemon
                        |> updateWith SinglePokemon GotPokemonMsg model

                _ ->
                    ( model, Cmd.none )

        ClickedLink urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model, Nav.pushUrl model.navKey <| Url.toString url )

                External href ->
                    ( model, Nav.load href )

        ChangedUrl url ->
            changeRouteTo (url |> Url.toString |> Route.toRoute) model


test : Model -> ( Model, Cmd Msg )
test model =
    ( model, Cmd.none )


updateWith : (subModel -> CurrentPage) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( { model | currentPage = toModel subModel }
    , Cmd.map toMsg subCmd
    )



--------------------------  VIEW --------------------------


view : Model -> Document Msg
view model =
    { title = "Pokemon"
    , body =
        [ div [ class "container-fluid" ]
            [ CDN.stylesheet
            , div [ class "row" ]
                [ div [ class "col-12" ]
                    [ navbar ]
                ]
            , case model.currentPage of
                Pokedex pokedex ->
                    Pokedex.view pokedex

                SinglePokemon singlePokemon ->
                    SinglePokemon.view singlePokemon
            ]
        ]
    }


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


changeRouteTo : Route.Route -> Model -> ( Model, Cmd Msg )
changeRouteTo route model =
    case route of
        Route.NotFound ->
            ( model, Cmd.none )

        Route.Pokedex ->
            Pokedex.init ()
                |> updateWith Pokedex GotPokedexMsg model

        Route.SinglePokemon id ->
            SinglePokemon.init () id
                |> updateWith SinglePokemon GotPokemonMsg model



-------------------------- MAIN -----------------


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }
