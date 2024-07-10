module Main exposing (main)

import Browser
import Css exposing (hover)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Model exposing (Model, ensureNoIllegalSelections, initModel, selected2Dshapes, selected3Dshapes, solveShapes)
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


updateSelections : Model -> Position -> Model.StatueSelection -> Model
updateSelections model pos newSelection =
    case pos of
        Left ->
            { model | leftStatueSelections = newSelection }

        Middle -
            { model | middleStatueSelections = newSelection }

        Right ->
            { model | rightStatueSelections = newSelection }


update : Msg -> Model -> Model
update msg model =
    let
        updateModel =
            updateSelections model

        newModel =
            case msg of
                SelectionInside pos shape ->
                    (getSelection pos >> newSelectionInside shape >> updateModel pos) model

                SelectionOutside pos shape ->
                    (getSelection pos >> newSelectionOutside shape >> updateModel pos) model

                ResetSelections ->
                    initModel

                NoOp ->
                    model
    in
    (ensureNoIllegalSelections >> solveShapes) newModel


view : Model -> Html Msg
view model =
    let
        selectedInsideShapes =
            selected2Dshapes model

        selectedOutsideShapes =
            selected3Dshapes model

        viewStatue =
            renderStatue selectedInsideShapes selectedOutsideShapes
    in
    main_
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
        [ div [ css [ Tw.flex, Tw.flex_col, Tw.items_center ] ]
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
                [ css [ Tw.flex, Tw.flex_col, Tw.gap_5 ] ]
                [ div
                    [ css [ Tw.flex, Tw.flex_wrap, Tw.flex_row, Tw.gap_10 ] ]
                    [ viewStatue Left model.leftStatueSelections
                    , viewStatue Middle model.middleStatueSelections
                    , viewStatue Right model.rightStatueSelections
                    ]
                , button
                    [ css
                        [ Tw.bg_color Theme.blue_500
                        , hover [ Tw.bg_color Theme.blue_700 ]
                        , Tw.border
                        , Tw.border_color Theme.blue_700
                        , Tw.text_color Theme.white
                        , Tw.font_bold
                        , Tw.py_2
                        , Tw.px_4
                        , Tw.rounded
                        , Tw.text_xl
                        ]
                    , onClick ResetSelections
                    ]
                    [ text "Reset Selections" ]
                ]
            , div
                [ css
                    [ Tw.flex
                    , Tw.flex_wrap
                    , Tw.py_12
                    , Tw.flex_col
                    , Tw.justify_center
                    , Tw.gap_10
                    ]
                ]
              <|
                renderSteps model.steps
            ]
        , footer
            [ css
                [ Tw.flex
                , Tw.justify_center
                ]
            ]
            [ p [] [ text "Created by GoldenGod#1001" ] ]
        ]
