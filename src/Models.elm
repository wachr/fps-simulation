module Models exposing (Facing(..), GameState, Player, makePlayer)


type Facing
    = North
    | East
    | West
    | South


type alias PlayerId =
    Int


type alias Player =
    { location : ( Int, Int ), facing : Facing, identifier : PlayerId }


type alias GameState =
    { gridSize : Int
    , players : List Player
    , selectedPlayerIdentifier : Maybe PlayerId
    }


makePlayer : ( Int, Int ) -> Facing -> List Player -> List Player
makePlayer location facing players =
    Player location facing (1 + List.length players) :: players
