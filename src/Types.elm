module Types exposing (..)

import Browser exposing (UrlRequest(..))
import Dict exposing (Dict)
import Http as Http
import Json.Decode as Decode exposing (Decoder, bool, dict, float, int, list, nullable, string)
import Json.Decode.Pipeline exposing (optional, required)
import Url.Parser exposing (int)


type RemoteData error value
    = NotAsked
    | Loading
    | Failure error
    | Success value


type alias WebData a =
    RemoteData Http.Error a


type Route
    = Pokedex
    | SinglePokemon Int
    | NotFound


type PokemonImage
    = Full
    | Sprite


type alias Pokemon =
    { id : Int
    , name : String
    , types : List String
    , generation : String
    }


pokemonDecoder : Decoder Pokemon
pokemonDecoder =
    Decode.succeed Pokemon
        |> required "id" Decode.int
        |> required "name" string
        |> required "types" (list string)
        |> required "generation" string


type alias FullPokemon =
    { id : Int
    , name : String
    , height : Float
    , weight : Float
    , baseExperience : Int
    , position : Int
    , isDefault : Bool
    , captureRate : Int
    , baseHappiness : Int
    , genderRate : Int
    , hatchCounter : Int
    , types : List String
    , abilities : List PokemonAbilities
    , moves : List PokemonMoves
    , efficacies : Dict String Float
    , stats : PokemonStats
    }


fullPokemonDecoder : Decoder FullPokemon
fullPokemonDecoder =
    Decode.succeed FullPokemon
        |> required "id" Decode.int
        |> required "name" string
        |> required "height" float
        |> required "weight" float
        |> required "baseExperience" Decode.int
        |> required "position" Decode.int
        |> required "isDefault" bool
        |> required "captureRate" Decode.int
        |> required "baseHappiness" Decode.int
        |> required "genderRate" Decode.int
        |> required "hatchCounter" Decode.int
        |> required "types" (list string)
        |> required "abilities" (list pokemonAbilitiesDecoder)
        |> required "moves" (list pokemonMovesDecoder)
        |> required "efficacies" (dict float)
        |> required "stats" pokemonStatsDecoder


type alias PokemonAbilities =
    { effect : String
    , isHidden : Bool
    , name : String
    , shortEffect : String
    , slot : Int
    }


pokemonAbilitiesDecoder : Decoder PokemonAbilities
pokemonAbilitiesDecoder =
    Decode.succeed PokemonAbilities
        |> required "effect" string
        |> required "isHidden" bool
        |> required "name" string
        |> required "shortEffect" string
        |> required "slot" Decode.int


type alias PokemonMoves =
    { level : Maybe Int
    , order : Maybe Int
    , damageClass : String
    , name : String
    , power : Maybe Int
    , accuracy : Maybe Int
    , moveType : Maybe String
    , learnMethods : String
    , tmMachineNumber : Int
    }


pokemonMovesDecoder : Decoder PokemonMoves
pokemonMovesDecoder =
    Decode.succeed PokemonMoves
        |> required "level" (nullable Decode.int)
        |> required "order" (nullable Decode.int)
        |> optional "damageClass" string "Invalid Damage Class."
        |> optional "name" string "Invalid Name."
        |> required "power" (nullable Decode.int)
        |> required "accuracy" (nullable Decode.int)
        |> required "type" (nullable string)
        |> optional "learnMethods" string "Invalid Learn Methods."
        |> required "tmMachineNumber" Decode.int


type alias PokemonStats =
    { hp : Int
    , attack : Int
    , defense : Int
    , spAttack : Int
    , spDefense : Int
    , speed : Int
    , total : Int
    }


pokemonStatsDecoder : Decoder PokemonStats
pokemonStatsDecoder =
    Decode.succeed PokemonStats
        |> required "hp" Decode.int
        |> required "attack" Decode.int
        |> required "defense" Decode.int
        |> required "spAttack" Decode.int
        |> required "spDefense" Decode.int
        |> required "speed" Decode.int
        |> required "total" Decode.int


type alias PokemonEvolutionChain =
    { id : Int
    , name : String
    , generation : String
    , evolutionCondition : String
    }


pokemonEvolutionChainDecoder : Decoder PokemonEvolutionChain
pokemonEvolutionChainDecoder =
    Decode.succeed PokemonEvolutionChain
        |> required "id" Decode.int
        |> optional "name" string "Invalid Name."
        |> required "generation" string
        |> optional "evolutionCondition" string ""


type alias RGBA =
    { red : Float
    , green : Float
    , blue : Float
    , alpha : Float
    }


type Stats
    = HP
    | Attack
    | Defense
    | SpAttack
    | SpDefense
    | Speed
    | Total
