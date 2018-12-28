module ControlPanel exposing
    ( drawControlPanelFromGameState
    , drawControlPanelView
    , drawGridSizeControl
    , drawPlayersControl
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
import Utilities exposing (..)


drawControlPanelView : GameState -> Html Msg
drawControlPanelView gameState =
    styled div
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
            , drawControlPanelFromGameState gameState
            ]
        ]


drawControlPanelFromGameState : GameState -> Html Msg
drawControlPanelFromGameState gameState =
    styled div
        [ Css.displayFlex
        , Css.flexDirection Css.column
        , Css.justifyContent Css.center
        , Css.alignItems Css.stretch
        ]
        []
        [ drawGridSizeControl gameState
        , drawPlayersControl gameState
        ]


drawGridSizeControl : GameState -> Html Msg
drawGridSizeControl { gridSize } =
    styled div
        [ Css.displayFlex
        , Css.flexDirection Css.column
        , Css.justifyContent Css.center
        , Css.alignItems Css.center
        , Css.fontSize (Css.pt 16)
        ]
        []
        [ text "Grid size:"
        , styled div
            [ Css.displayFlex
            , Css.flexDirection Css.row
            , Css.justifyContent Css.center
            ]
            []
            [ styled button [] [ onClick GridDecrement ] [ text "-" ]
            , styled div [ Css.margin2 zero (em 1) ] [] [ text (String.fromInt gridSize) ]
            , styled button [] [ onClick GridIncrement ] [ text "+" ]
            ]
        ]


drawPlayersControl : GameState -> Html Msg
drawPlayersControl { players, selectedPlayerIdentifier } =
    styled div
        [ Css.displayFlex
        , Css.flexDirection Css.column
        , Css.justifyContent Css.center
        , Css.alignItems Css.center
        , Css.fontSize (Css.pt 16)
        ]
        []
        [ text "Players:"
        , styled div
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Css.justifyContent Css.flexStart
            ]
            []
          <|
            (List.intersperse (styled br [] [] []) <|
                List.map (drawPlayerInfo selectedPlayerIdentifier) <|
                    List.sortBy .identifier players
            )
                ++ (selectedPlayerIdentifier
                        |> Maybe.map
                            (\s ->
                                [ styled button [ Css.property "background-color" "green" ] [ onClick RotateClockwise ] [ text "Turn" ] ]
                            )
                        |> Maybe.withDefault []
                   )
        ]


drawPlayerInfo : Maybe Int -> Player -> Html Msg
drawPlayerInfo selectedPlayerIdentifier player =
    String.join " "
        [ "Player"
        , String.fromInt <| player.identifier
        , ":"
        , case player.facing of
            North ->
                String.fromChar '⇧'

            East ->
                String.fromChar '⇨'

            South ->
                String.fromChar '⇩'

            West ->
                String.fromChar '⇦'
        , "("
        , String.fromInt << Tuple.first <| player.location
        , ","
        , String.fromInt << Tuple.second <| player.location
        , ")"
        ]
        |> text
        |> List.singleton
        |> styled span
            (selectedPlayerIdentifier
                |> Maybe.andThen (filterBy ((==) player.identifier))
                |> Maybe.map (\_ -> [ Css.property "background-color" "Gold" ])
                |> withDefault []
            )
            (selectedPlayerIdentifier
                |> Maybe.andThen (filterBy ((==) player.identifier))
                |> Maybe.map (\_ -> DeselectPlayer)
                |> withDefault (SelectPlayer player.identifier)
                |> onClick
                |> List.singleton
            )
