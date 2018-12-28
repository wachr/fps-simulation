module Messages exposing (Msg(..))


type Msg
    = GridIncrement
    | GridDecrement
    | SelectPlayer Int
    | DeselectPlayer
    | RotateClockwise
