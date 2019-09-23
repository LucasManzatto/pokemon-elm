module Api exposing (..)

import Browser exposing (Document, UrlRequest(..))
import Http exposing (get)
import Json.Decode as Decode exposing (list)
import Platform.Cmd exposing (Cmd)
import Types exposing (..)
import Url exposing (Url)
import Utils exposing (..)


apiUrl : String
apiUrl =
    -- "https://localhost:5001/api"
    "http://localhost:58803/api"


pokemonsUrl : String
pokemonsUrl =
    apiUrl ++ "/Pokemons/"


type Msg
    = PokemonsReceived (WebData (List Pokemon))
    | FullPokemonReceived (WebData FullPokemon)
    | PokemonEvolutionChainReceived (WebData (List PokemonEvolutionChain))
    | MaxStatsReceived (WebData PokemonStats)


getMaxStats : Cmd Msg
getMaxStats =
    Http.get
        { url = pokemonsUrl ++ "maxStats"
        , expect =
            Http.expectJson (fromResult >> MaxStatsReceived) pokemonStatsDecoder
        }


getPokemon : Int -> Cmd Msg
getPokemon pokemonId =
    Http.get
        { url = pokemonsUrl ++ String.fromInt pokemonId
        , expect = Http.expectJson (fromResult >> FullPokemonReceived) fullPokemonDecoder
        }


getPokemonEvolutionChain : Int -> Cmd Msg
getPokemonEvolutionChain pokemonId =
    Http.get
        { url = pokemonsUrl ++ String.fromInt pokemonId ++ "/evolutionChain"
        , expect = Http.expectJson (fromResult >> PokemonEvolutionChainReceived) (Decode.list pokemonEvolutionChainDecoder)
        }


getPokedex : Cmd Msg
getPokedex =
    Http.get
        { url = pokemonsUrl
        , expect = Http.expectJson (fromResult >> PokemonsReceived) (Decode.list pokemonDecoder)
        }
