module Messages exposing (Msg(..))

import Models exposing (..)


type Msg
    = GridIncrement
    | GridDecrement
    | SelectPlayer Int
    | DeselectPlayer
    | UpdatePlayer Player
    | FaceRandom
