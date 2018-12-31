module Simulation exposing
    ( drawGridCell
    , drawGridFromGameState
    , drawGridPlayer
    , drawGridRowFromGameState
    , drawSimulationView
    , gridCell
    , gridCellBoundary
    , gridContainer
    , gridRow
    )

import Browser
import Css exposing (auto, calc, em, minus, pct, px, rem, vh, vw, zero)
import Css.Global exposing (body, global)
import Debug
import Html
import Html.Styled exposing (Html, br, button, div, fromUnstyled, h1, hr, span, styled, text, toUnstyled)
import Html.Styled.Attributes as Html exposing (id)
import Html.Styled.Events exposing (onClick)
import List
import Maybe exposing (Maybe(..), withDefault)
import Messages exposing (..)
import Models exposing (..)
import Svg.Styled as Svg exposing (Svg, svg)
import Svg.Styled.Attributes as Svg


drawSimulationView : GameState -> Html Msg
drawSimulationView gameState =
    styled div
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


drawGridPlayer : Bool -> Player -> Html Msg
drawGridPlayer selectedPlayer { facing, identifier } =
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

        highlightPlayerIfSelected =
            if selectedPlayer then
                [ Svg.stroke "Black"
                , Svg.strokeWidth "10px"
                , Svg.strokeDasharray "157,157"
                , Svg.strokeDashoffset "79"
                ]

            else
                []
    in
    styled div
        [ Css.width (pct 75)
        , Css.height (pct 75)
        , Css.justifyContent Css.center
        , Css.alignItems Css.center
        , Css.displayFlex
        ]
        [ Html.class "grid-player"
        , onClick <|
            if selectedPlayer then
                DeselectPlayer

            else
                SelectPlayer identifier
        ]
        [ Svg.styled svg
            []
            [ Svg.viewBox "-5 -5 310 310" ]
            [ Svg.circle
                (List.append
                    [ Svg.cx "150"
                    , Svg.cy "150"
                    , Svg.r "150"
                    , Svg.fill "ForestGreen"
                    ]
                    highlightPlayerIfSelected
                )
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


drawGridCell : ( Int, Int ) -> Bool -> List Player -> Html Msg
drawGridCell coordinate selectedPlayer players =
    List.filter ((==) coordinate << .location) players
        |> List.head
        |> Maybe.map (drawGridPlayer selectedPlayer)
        |> Maybe.map List.singleton
        |> withDefault []
        |> styled div
            [ gridCell ]
            [ Html.class "grid-cell" ]


drawGridRowFromGameState : GameState -> Int -> Html Msg
drawGridRowFromGameState { gridSize, selectedPlayerIdentifier, players } rowIndex =
    List.range 1 gridSize
        |> List.map
            (\columnIndex ->
                players
                    |> List.filter
                        ((==) rowIndex
                            << Tuple.second
                            << .location
                        )
                    |> drawGridCell
                        ( columnIndex, rowIndex )
                        (players
                            |> List.filter (.location >> (==) ( columnIndex, rowIndex ))
                            |> List.any
                                (\player ->
                                    selectedPlayerIdentifier
                                        |> Maybe.map ((==) player.identifier)
                                        |> withDefault False
                                )
                        )
            )
        |> styled div
            [ gridRow ]
            [ Html.class "grid-row" ]


drawGridFromGameState : GameState -> Html Msg
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
