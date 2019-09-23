module Main exposing (..)

import Api as Api exposing(..)
import Bootstrap.CDN as CDN
import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav
import Debug exposing (log)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Pages.Pokedex as PokedexPage exposing (..)
import Pages.SinglePokemon as SinglePokemonPage exposing (..)
import Platform.Cmd exposing (Cmd)
import Route exposing (Route)
import Time
import Types exposing (..)
import Url exposing (Url)
import Utils exposing (..)


type alias Model =
    { currentPage : CurrentPage
    , navKey : Nav.Key
    , route : Route
    , maxStats : WebData PokemonStats
    }


type CurrentPage
    = Pokedex PokedexPage.Model
    | SinglePokemon SinglePokemonPage.Model


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    ({currentPage = Pokedex PokedexPage.initPokedex, navKey = navKey,route = url |> Url.toString |> Route.toRoute ,maxStats = NotAsked}
    , Cmd.batch [ Cmd.map GotApiMsg Api.getMaxStats])



-------------------------- UPDATE --------------------


type Msg
    = ChangedUrl Url
    | ClickedLink UrlRequest
    | GotPokedexMsg PokedexPage.Msg
    | GotPokemonMsg SinglePokemonPage.Msg
    | GotApiMsg Api.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPokedexMsg subMsg ->
            case model.currentPage of
                Pokedex pokedexModel ->
                    PokedexPage.update subMsg {pokedexModel | maxStats = model.maxStats}
                        |> updateWith Pokedex GotPokedexMsg model

                _ ->
                    ( model, Cmd.none )

        GotPokemonMsg subMsg ->
            case model.currentPage of
                SinglePokemon singlePokemonModel ->
                    SinglePokemonPage.update subMsg {singlePokemonModel | maxStats = model.maxStats}
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

        GotApiMsg apiMsg ->
            case apiMsg of
                Api.MaxStatsReceived response->
                    changeRouteTo model.route {model | maxStats = response}
                _ ->
                    (model,Cmd.none)



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
                    Html.map GotPokedexMsg (PokedexPage.view pokedex)

                SinglePokemon singlePokemon ->
                    SinglePokemonPage.view singlePokemon
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
            PokedexPage.init ()
                |> updateWith Pokedex GotPokedexMsg model

        Route.SinglePokemon id ->
            SinglePokemonPage.init () id 
                |> updateWith SinglePokemon GotPokemonMsg model



-------------------------- MAIN -----------------


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.currentPage of
        Pokedex pokedexModel ->
            Sub.map GotPokedexMsg (PokedexPage.subscriptions pokedexModel)

        _ ->
            Sub.none


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }
