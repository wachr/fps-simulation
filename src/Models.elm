module Models exposing (Facing(..), GameState, Player, makePlayer)

import Messages exposing (..)

type Facing
    = North
    | East
    | West
    | South


type alias Player =
    { location : ( Int, Int ), facing : Facing, identifier : Int }


type alias GameState =
    { gridSize : Int
    , players : List Player
    , selectedPlayerIdentifier : Maybe Int
    }


makePlayer : ( Int, Int ) -> Facing -> List Player -> List Player
makePlayer location facing players =
    Player location facing (1 + List.length players) :: players
