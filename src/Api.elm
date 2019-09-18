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
    "http://localhost:58803/api"


type ApiMsg
    = PokemonsReceived (WebData (List Pokemon))
    | FullPokemonReceived (WebData FullPokemon)
    | PokemonEvolutionChainReceived (WebData (List PokemonEvolutionChain))
    | MaxStatsReceived (WebData PokemonStats)


getMaxStats : Cmd ApiMsg
getMaxStats =
    Http.get
        { url = apiUrl ++ "/Pokemons/maxStats"
        , expect =
            Http.expectJson (fromResult >> MaxStatsReceived) pokemonStatsDecoder
        }


getPokemon : Int -> Cmd ApiMsg
getPokemon pokemonId =
    Http.get
        { url = apiUrl ++ "/Pokemons/" ++ String.fromInt pokemonId
        , expect = Http.expectJson (fromResult >> FullPokemonReceived) fullPokemonDecoder
        }


getPokemonEvolutionChain : Int -> Cmd ApiMsg
getPokemonEvolutionChain pokemonId =
    Http.get
        { url = apiUrl ++ "/Pokemons/" ++ String.fromInt pokemonId ++ "/evolutionChain"
        , expect = Http.expectJson (fromResult >> PokemonEvolutionChainReceived) (Decode.list pokemonEvolutionChainDecoder)
        }
