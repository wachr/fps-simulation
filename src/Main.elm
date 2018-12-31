module FpsSimulation exposing (main, view)

import Browser
import ControlPanel
import Css exposing (pct, vh)
import Css.Global exposing (body, global)
import Debug
import Html.Styled exposing (Html, div, styled, toUnstyled)
import List
import Messages exposing (..)
import Models exposing (..)
import Random
import Simulation
import Utilities exposing (..)


main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( GameState, Cmd a )
init _ =
    ( { gridSize = 3
      , players = makePlayer ( 1, 1 ) West <| makePlayer ( 3, 2 ) North []
      , selectedPlayerIdentifier = Nothing
      }
    , Cmd.none
    )


update : Msg -> GameState -> ( GameState, Cmd Msg )
update msg gameState =
    case msg of
        GridIncrement ->
            ( { gameState | gridSize = gameState.gridSize + 1 }, Cmd.none )

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
            ( { gameState
                | gridSize = newGridSize
                , players = clampPlayerLocations gameState.players --TODO If multple players end up at the same location, choose a victor and remove the others.
              }
            , Cmd.none
            )

        SelectPlayer identifier ->
            ( { gameState | selectedPlayerIdentifier = Just identifier }, Cmd.none )

        DeselectPlayer ->
            ( { gameState | selectedPlayerIdentifier = Nothing }, Cmd.none )

        UpdatePlayer newPlayer ->
            ( updatePlayer gameState newPlayer, Cmd.none )

        FaceRandom ->
            ( gameState
            , Random.generate
                (\newFacing ->
                    gameState.selectedPlayerIdentifier
                        |> Maybe.map (\id -> List.filter (.identifier >> (==) id) gameState.players)
                        |> Maybe.andThen List.head
                        |> Maybe.withDefault (Player ( -1, -1 ) North -1)
                        -- FIXME This is a hack relying on a player value we are pretty sure is not in the players list
                        |> (\player -> { player | facing = newFacing })
                        >> UpdatePlayer
                )
                randomFacing
            )


randomFacing : Random.Generator Facing
randomFacing =
    Random.uniform North [ East, West, South ]


updatePlayer : GameState -> Player -> GameState
updatePlayer gameState player =
    let
        newPlayers =
            List.map
                (\p ->
                    if p.identifier == player.identifier then
                        player

                    else
                        p
                )
                gameState.players
    in
    { gameState | players = newPlayers }


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
        , ControlPanel.drawControlPanelView gameState
        , Simulation.drawSimulationView gameState
        ]
