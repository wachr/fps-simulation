module FpsSimulation exposing (main, view)

import Browser
import ControlPanel exposing (..)
import Css exposing (pct, vh)
import Css.Global exposing (body, global)
import Debug
import Html.Styled exposing (Html, div, styled, toUnstyled)
import List
import Messages exposing (..)
import Models exposing (..)
import Simulation exposing (..)


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view >> toUnstyled
        }


init : GameState
init =
    { gridSize = 3
    , players = makePlayer ( 1, 1 ) West <| makePlayer ( 3, 2 ) North []
    , selectedPlayerIdentifier = Nothing
    }


update : Msg -> GameState -> GameState
update msg gameState =
    case msg of
        GridIncrement ->
            { gameState | gridSize = gameState.gridSize + 1 }

        GridDecrement ->
            let
                newGridSize =
                    gameState.gridSize - 1

                clampPlayerLocations : List Player -> List Player
                clampPlayerLocations =
                    List.map
                        (\player ->
                            { player
                                | location =
                                    player.location
                                        |> Tuple.mapFirst (clamp 1 newGridSize)
                                        |> Tuple.mapSecond (clamp 1 newGridSize)
                            }
                        )
            in
            { gameState
                | gridSize = newGridSize
                , players = clampPlayerLocations gameState.players --TODO If multple players end up at the same location, choose a victor and remove the others.
            }

        SelectPlayer identifier ->
            { gameState | selectedPlayerIdentifier = Just identifier }

        DeselectPlayer ->
            { gameState | selectedPlayerIdentifier = Nothing }


view : GameState -> Html Msg
view gameState =
    styled div
        [ Css.height (vh 95) ]
        []
        [ global
            [ body
                [ Css.property "background-color" "Seashell"
                , Css.backgroundSize (pct 100)
                ]
            ]
        , drawControlPanelView gameState
        , drawSimulationView gameState
        ]
