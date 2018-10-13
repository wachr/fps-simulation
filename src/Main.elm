module FpsSimulation exposing (main, view)

import Browser
import Css exposing (calc, em, minus, pct, px, vh)
import Css.Global exposing (body, global)
import Html
import Html.Styled exposing (Html, div, fromUnstyled, h1, hr, styled, text, toUnstyled)
import Html.Styled.Attributes as Html exposing (id)


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view >> toUnstyled
        }


type alias Model =
    {}


init : Model
init =
    {}


update : msg -> Model -> Model
update msg gameState =
    gameState


view : Model -> Html msg
view gameState =
    styled div
        [ Css.height (vh 95) ]
        []
        [ global
            [ body
                [ Css.backgroundColor (Css.rgb 223 207 159)
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
                [ Css.margin Css.zero
                , Css.textAlign Css.center
                ]
                []
                [ text "Control panel", hr [] [] ]
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
                [ Css.margin Css.zero
                , Css.textAlign Css.center
                ]
                []
                [ text "Simulation pane", hr [] [] ]
            ]
        ]
