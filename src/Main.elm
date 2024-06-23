module Main exposing (main)

import Browser
import Html.Events exposing (onClick)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Model exposing (Model, initModel)
import Msg exposing (Msg(..))
import Shapes exposing (Shape2D, Shape3D, toString2D, toString3D)
import Statues.Internal exposing (Position(..))
import Svg.Styled
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import View.Shapes exposing (circle)
import View.Statues exposing (renderStatue)


main =
    Browser.sandbox { init = initModel, update = update, view = view >> toUnstyled }


update : Msg -> Model -> Model
update msg model =
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
            in
            case pos of
                Left ->
                    { model | leftStatueSelections = newSelection }

                Middle ->
                    { model | middleStatueSelections = newSelection }

                Right ->
                    { model | rightStatueSelections = newSelection }

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
            in
            case pos of
                Left ->
                    { model | leftStatueSelections = newSelection }

                Middle ->
                    { model | middleStatueSelections = newSelection }

                Right ->
                    { model | rightStatueSelections = newSelection }

        NoOp ->
            model


viewModel : Model -> Html Msg
viewModel model =
    let
        left =
            case ( model.leftStatueSelections.insideShape, model.leftStatueSelections.outsideShape ) of
                ( Just inside, Just outside ) ->
                    text (toString2D inside ++ " && " ++ toString3D outside)

                _ ->
                    text "none"

        middle =
            case ( model.middleStatueSelections.insideShape, model.middleStatueSelections.outsideShape ) of
                ( Just inside, Just outside ) ->
                    text (toString2D inside ++ " && " ++ toString3D outside)

                _ ->
                    text "none"

        right =
            case ( model.rightStatueSelections.insideShape, model.rightStatueSelections.outsideShape ) of
                ( Just inside, Just outside ) ->
                    text (toString2D inside ++ " && " ++ toString3D outside)

                _ ->
                    text "none"
    in
    div [ css [ Tw.text_color Theme.white ] ] [ left, middle, right ]


view : Model -> Html Msg
view model =
    main_
        [ css
            [ Tw.bg_color Theme.zinc_900
            , Bp.lg [ Tw.px_40 ]
            , Bp.md [ Tw.px_20 ]
            , Bp.sm [ Tw.px_10 ]
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
            [ text "Salvation's Edge Fourth Encounter: Verity" ]
        , div
            [ css [ Tw.flex, Tw.flex_wrap, Bp.lg [ Tw.flex_row ], Bp.md [ Tw.flex_col ], Bp.sm [ Tw.flex_col ], Tw.justify_center, Tw.gap_10 ] ]
            [ renderStatue Left
            , renderStatue Middle
            , renderStatue Right
            ]
        , viewModel model
        ]
