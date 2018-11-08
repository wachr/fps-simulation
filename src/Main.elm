module FpsSimulation exposing (main, view)

import Browser
import Css exposing (auto, calc, em, minus, pct, px, rem, vh, vw, zero)
import Css.Global exposing (body, global)
import Html
import Html.Styled exposing (Html, div, h1, hr, styled, text, toUnstyled)
import Html.Styled.Attributes as Html exposing (id)
import List
import Maybe exposing (withDefault)
import Svg.Styled as Svg exposing (Svg, svg)
import Svg.Styled.Attributes as Svg


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view >> toUnstyled
        }


type Facing
    = North
    | East
    | West
    | South


type alias Player =
    { location : ( Int, Int ), facing : Facing }


type alias GameState =
    { gridSize : Int
    , players : List Player
    }


init : GameState
init =
    { gridSize = 10
    , players = [ Player ( 4, 3 ) West, Player ( 8, 6 ) North ]
    }


update : msg -> GameState -> GameState
update msg gameState =
    gameState


view : GameState -> Html msg
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
        , styled div
            [ Css.border2 (Css.px 1) Css.solid
            , Css.boxSizing Css.borderBox
            , Css.padding (Css.px 10)
            , Css.height (pct 100)
            , Css.float Css.right
            , Css.width <| calc (pct 30) minus (em 1)
            ]
            [ id "control-panel" ]
            [ styled h1
                [ Css.margin zero
                , Css.textAlign Css.center
                ]
                []
                [ text "Control panel"
                , hr [] []
                ]
            ]
        , styled div
            [ Css.border2 (Css.px 1) Css.solid
            , Css.boxSizing Css.borderBox
            , Css.padding (Css.px 10)
            , Css.height (pct 100)
            , Css.float Css.left
            , Css.width (pct 70)
            ]
            [ id "simulation-frame" ]
            [ styled h1
                [ Css.margin zero
                , Css.textAlign Css.center
                , Css.boxSizing Css.borderBox
                ]
                []
                [ text "Simulation pane"
                , hr [] []
                , drawGridFromGameState gameState
                ]
            ]
        ]


drawGridPlayer : Player -> Html msg
drawGridPlayer { facing } =
    let
        transformationForFacing =
            case facing of
                North ->
                    Svg.transform "rotate(0 150 150)"

                East ->
                    Svg.transform "rotate(90 150 150)"

                South ->
                    Svg.transform "rotate(180 150 150)"

                West ->
                    Svg.transform "rotate(270 150 150)"
    in
    styled div
        []
        [ Html.class "grid-player" ]
        [ Svg.styled svg
            []
            [ Svg.viewBox "0 0 300 300" ]
            [ Svg.circle
                [ Svg.cx "150"
                , Svg.cy "150"
                , Svg.r "150"
                , Svg.fill "ForestGreen"
                ]
                []
            , Svg.polygon
                [ Svg.height "300"
                , Svg.width "300"
                , Svg.fill "DarkSlateGray"
                , Svg.points "20,225 150,0 280,225"
                , transformationForFacing
                ]
                []
            ]
        ]


drawGridCell : ( Int, Int ) -> List Player -> Html msg
drawGridCell coordinate players =
    let
        playerDiv : Maybe (Html msg)
        playerDiv =
            List.filter ((==) coordinate << .location) players
                |> List.head
                >> Maybe.map drawGridPlayer
    in
    styled div
        [ gridCell ]
        [ Html.class "grid-cell" ]
    <|
        withDefault
            []
        <|
            Maybe.map
                List.singleton
                playerDiv


drawGridRowFromGameState : GameState -> Int -> Html msg
drawGridRowFromGameState { gridSize, players } rowIndex =
    List.range 1 gridSize
        |> List.map
            (\columnIndex ->
                players
                    |> List.filter
                        ((==) rowIndex
                            << Tuple.second
                            << .location
                        )
                    |> drawGridCell ( columnIndex, rowIndex )
            )
        |> styled div
            [ gridRow ]
            [ Html.class "grid-row" ]


drawGridFromGameState : GameState -> Html msg
drawGridFromGameState gameState =
    List.range 1 gameState.gridSize
        |> List.map (drawGridRowFromGameState gameState)
        |> styled div
            [ gridContainer ]
            [ Html.class "grid-container" ]


gridContainer : Css.Style
gridContainer =
    Css.batch
        [ Css.margin2 zero auto
        , Css.padding gridCellBoundary
        , Css.width (vw 80)
        , Css.height (vh 80)
        , Css.maxWidth (vh 80)
        , Css.maxHeight (vw 80)
        , Css.property "background-color" "DodgerBlue"
        ]


gridRow : Css.Style
gridRow =
    Css.displayFlex


gridCell : Css.Style
gridCell =
    Css.batch
        [ Css.flex (Css.int 1)
        , Css.margin gridCellBoundary
        , Css.property "background-color" "Moccasin"
        , Css.justifyContent Css.center
        , Css.alignItems Css.center
        , Css.displayFlex
        , Css.after
            [ Css.float Css.left
            , Css.paddingTop (pct 100)
            , Css.property "content" "''"
            ]
        ]


gridCellBoundary =
    rem 0.1
