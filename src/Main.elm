module FpsSimulation exposing (main, view)

import Browser
import Css exposing (auto, calc, em, minus, pct, px, rem, vh, vw, zero)
import Css.Global exposing (body, global)
import Html
import Html.Styled exposing (Html, br, button, div, fromUnstyled, h1, hr, span, styled, text, toUnstyled)
import Html.Styled.Attributes as Html exposing (id)
import Html.Styled.Events exposing (onClick)
import List
import Maybe exposing (Maybe(..), withDefault)
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
    , selectedPlayerIndex : Maybe Int
    }


init : GameState
init =
    { gridSize = 3
    , players = [ Player ( 1, 1 ) West, Player ( 3, 2 ) North ]
    , selectedPlayerIndex = Just 0
    }


type Msg
    = GridIncrement
    | GridDecrement
    | SelectPlayer Int
    | DeselectPlayer


update : Msg -> GameState -> GameState
update msg gameState =
    case msg of
        GridIncrement ->
            { gameState | gridSize = gameState.gridSize + 1 }

        GridDecrement ->
            { gameState | gridSize = gameState.gridSize - 1 }

        SelectPlayer newIndex ->
            { gameState | selectedPlayerIndex = Just newIndex }

        DeselectPlayer ->
            { gameState | selectedPlayerIndex = Nothing }


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
drawPlayersControl { players, selectedPlayerIndex } =
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
            List.intersperse (styled br [] [] []) <|
                List.indexedMap
                    (\currentIndex player ->
                        String.join " "
                            [ "Player"
                            , String.fromInt <| currentIndex + 1
                            , ":"
                            , "("
                            , String.fromInt << Tuple.first <| player.location
                            , ","
                            , String.fromInt << Tuple.second <| player.location
                            , ")"
                            ]
                            |> text
                            |> List.singleton
                            |> styled span
                                (selectedPlayerIndex
                                    |> Maybe.andThen (maybeFilter ((==) currentIndex))
                                    |> Maybe.map (\_ -> [ Css.property "background-color" "Gold" ])
                                    |> withDefault []
                                )
                                (selectedPlayerIndex
                                    |> Maybe.andThen (maybeFilter ((==) currentIndex))
                                    |> Maybe.map (\_ -> DeselectPlayer)
                                    |> withDefault (SelectPlayer currentIndex)
                                    |> onClick
                                    |> List.singleton
                                )
                    )
                    players
        ]


drawGridPlayer : Bool -> Player -> Html Msg
drawGridPlayer selectedPlayer { facing } =
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
        [ Html.class "grid-player" ]
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
drawGridRowFromGameState { gridSize, selectedPlayerIndex, players } rowIndex =
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
                        (selectedPlayerIndex
                            |> Maybe.map (\index -> List.drop index players)
                            |> Maybe.andThen List.head
                            |> Maybe.map .location
                            |> Maybe.map ((==) ( columnIndex, rowIndex ))
                            |> withDefault False
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


maybeFilter : (a -> Bool) -> a -> Maybe a
maybeFilter predicate operand =
    if predicate operand then
        Just operand

    else
        Nothing
