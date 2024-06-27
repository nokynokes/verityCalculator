module Main exposing (main)

import Browser
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Model exposing (Model, ensureLimit2DShapes, ensureNoIllegalSelections, ensureUniqueInsideShapes, initModel, solveShapes)
import Msg exposing (Msg(..))
import Statues.Internal exposing (Position(..))
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import View.Statues exposing (renderStatue)
import View.Steps exposing (renderSteps)


main : Program () Model Msg
main =
    Browser.sandbox { init = initModel, update = update, view = view >> toUnstyled }


update : Msg -> Model -> Model
update msg model =
    let
        newModel =
            case msg of
                SelectionInside pos shape ->
                    let
                        selection =
                            case pos of
                                Left ->
                                    model.leftStatueSelections

                                Middle ->
                                    model.middleStatueSelections

                                Right ->
                                    model.rightStatueSelections

                        newSelection =
                            { selection | insideShape = Just shape }

                        ensureUniquenessAfterUpdate =
                            ensureUniqueInsideShapes pos
                    in
                    case pos of
                        Left ->
                            ensureUniquenessAfterUpdate { model | leftStatueSelections = newSelection }

                        Middle ->
                            ensureUniquenessAfterUpdate { model | middleStatueSelections = newSelection }

                        Right ->
                            ensureUniquenessAfterUpdate { model | rightStatueSelections = newSelection }

                SelectionOutside pos shape ->
                    let
                        selection =
                            case pos of
                                Left ->
                                    model.leftStatueSelections

                                Middle ->
                                    model.middleStatueSelections

                                Right ->
                                    model.rightStatueSelections

                        newSelection =
                            { selection | outsideShape = Just shape }

                        ensureLimitsOnOutsideShapes =
                            ensureLimit2DShapes pos
                    in
                    case pos of
                        Left ->
                            ensureLimitsOnOutsideShapes { model | leftStatueSelections = newSelection }

                        Middle ->
                            ensureLimitsOnOutsideShapes { model | middleStatueSelections = newSelection }

                        Right ->
                            ensureLimitsOnOutsideShapes { model | rightStatueSelections = newSelection }

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
            , Bp.sm [ Tw.px_5 ]
            , Tw.scroll_smooth
            , Tw.font_serif
            , Tw.min_h_screen
            ]
        ]
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
            [ css [ Tw.flex, Tw.flex_wrap, Bp.lg [ Tw.flex_row ], Bp.md [ Tw.flex_col ], Bp.sm [ Tw.flex_col ], Tw.justify_between, Tw.gap_10 ] ]
            [ renderStatue Left model.leftStatueSelections
            , renderStatue Middle model.middleStatueSelections
            , renderStatue Right model.rightStatueSelections
            ]
        , div
            [ css [ Tw.flex, Tw.flex_wrap, Tw.py_12, Bp.xxl [ Tw.flex_col ], Bp.xl [ Tw.flex_col ], Bp.lg [ Tw.flex_col ], Bp.md [ Tw.flex_row ], Bp.sm [ Tw.flex_row ], Tw.justify_center, Tw.gap_10 ] ]
            (renderSteps model.steps)
        ]
