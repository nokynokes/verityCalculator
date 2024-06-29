module Main exposing (main)

import Browser
import Dict exposing (update)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Model exposing (Model, ensureLimit2DShapes, ensureNoIllegalSelections, ensureUniqueInsideShapes, initModel, solveShapes)
import Msg exposing (Msg(..))
import Shapes exposing (Shape2D, Shape3D)
import Statues.Internal exposing (Position(..))
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import View.Statues exposing (renderStatue)
import View.Steps exposing (renderSteps)


main : Program () Model Msg
main =
    Browser.sandbox { init = initModel, update = update, view = view >> toUnstyled }


getSelection : Position -> Model -> Model.StatueSelection
getSelection pos model =
    case pos of
        Left ->
            model.leftStatueSelections

        Middle ->
            model.middleStatueSelections

        Right ->
            model.rightStatueSelections


newSelectionInside : Shape2D -> Model.StatueSelection -> Model.StatueSelection
newSelectionInside shape selection =
    { selection | insideShape = Just shape }


newSelectionOutside : Shape3D -> Model.StatueSelection -> Model.StatueSelection
newSelectionOutside shape selection =
    { selection | outsideShape = Just shape }


updateSelections : Model -> Position -> (Position -> Model -> Model) -> Model.StatueSelection -> Model
updateSelections model pos ensure newSelection =
    let
        verify =
            ensure pos
    in
    case pos of
        Left ->
            verify { model | leftStatueSelections = newSelection }

        Middle ->
            verify { model | middleStatueSelections = newSelection }

        Right ->
            verify { model | rightStatueSelections = newSelection }


update : Msg -> Model -> Model
update msg model =
    let
        updateModel =
            updateSelections model

        newModel =
            case msg of
                SelectionInside pos shape ->
                    (getSelection pos >> newSelectionInside shape >> updateModel pos ensureUniqueInsideShapes) model

                SelectionOutside pos shape ->
                    (getSelection pos >> newSelectionOutside shape >> updateModel pos ensureLimit2DShapes) model

                NoOp ->
                    model
    in
    (ensureNoIllegalSelections >> solveShapes) newModel


view : Model -> Html Msg
view model =
    div
        [ css
            [ Tw.bg_color Theme.zinc_900
            , Bp.xxl [ Tw.px_80 ]
            , Bp.xl [ Tw.px_40 ]
            , Bp.lg [ Tw.px_20 ]
            , Bp.md [ Tw.px_10 ]
            , Tw.px_5
            , Tw.scroll_smooth
            , Tw.font_sans
            , Tw.min_h_screen
            , Tw.text_color Theme.white
            ]
        ]
        [ main_ []
            [ section
                [ css
                    [ Tw.text_5xl
                    , Tw.text_center
                    , Tw.text_color Theme.amber_100
                    , Tw.py_10
                    ]
                ]
                [ h1 [] [ text "Salvation's Edge Fourth Encounter: Verity" ] ]
            , div
                [ css [ Tw.flex, Tw.flex_wrap, Tw.flex_row, Tw.justify_center, Tw.gap_10 ] ]
                [ renderStatue Left model.leftStatueSelections
                , renderStatue Middle model.middleStatueSelections
                , renderStatue Right model.rightStatueSelections
                ]
            , div
                [ css [ Tw.flex, Tw.flex_wrap, Tw.py_12, Bp.xxl [ Tw.flex_col ], Bp.xl [ Tw.flex_col ], Bp.lg [ Tw.flex_col ], Bp.md [ Tw.flex_row ], Bp.sm [ Tw.flex_row ], Tw.justify_center, Tw.gap_10 ] ]
                (renderSteps model.steps)
            ]
        , footer [ css [ Tw.flex, Tw.justify_center ] ] [ p [] [ text "Created by GoldenGod#1001" ] ]
        ]
