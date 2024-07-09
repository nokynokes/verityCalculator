module View.Statues exposing (renderStatue)

import Css exposing (hover, position)
import Css.Global
import Html.Styled as Html exposing (Html, div, h2, hr, text)
import Html.Styled.Attributes as Html exposing (checked, css)
import Html.Styled.Events exposing (onCheck)
import Model exposing (StatueSelection, maxNumberOf2DShapes, numberOfCircles, numberOfSquares, numberOfTriangles)
import Msg exposing (Msg(..))
import Shapes exposing (Shape2D(..), Shape3D(..), isIllegalShapeToStart, toString2D, toString3D)
import Statues.Internal exposing (Position(..), toString)
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw


radioButton : Bool -> Bool -> Position -> Msg -> String -> String -> Html Msg
radioButton isSelected isDisabled position message className name =
    let
        id =
            toString position ++ name
    in
    Html.div
        [ css
            [ Tw.flex
            , Tw.flex_col
            ]
        , Html.class className
        ]
        [ Html.input
            [ Html.type_ "radio"
            , Html.name className
            , Html.id id
            , Html.checked isSelected
            , Html.disabled (not isSelected && isDisabled)
            , onCheck
                (\checked ->
                    if checked then
                        message

                    else
                        NoOp
                )
            , Html.css
                [ Tw.hidden
                , Css.checked
                    [ Css.Global.generalSiblings
                        [ Css.Global.selector ".radio-button__label"
                            [ Css.backgroundColor <| Css.hex "88c0d0" ]
                        ]
                    ]
                , Css.disabled
                    [ Css.Global.generalSiblings
                        [ Css.Global.selector ".radio-button__label"
                            [ Tw.text_color Theme.gray_400 ]
                        ]
                    ]
                ]
            ]
            []
        , Html.label
            [ Html.class "radio-button__label"
            , Html.for id
            , Html.css
                [ Tw.flex
                , Tw.items_center
                , Tw.justify_center
                , Tw.p_4
                , Tw.cursor_pointer
                , hover [ Tw.text_color Theme.amber_100, Tw.bg_color Theme.slate_500 ]
                ]
            ]
            [ text name ]
        ]


radioButtonGroup : List (Html msg) -> Html msg
radioButtonGroup =
    Html.div
        [ Html.css
            [ Tw.flex
            , Tw.justify_end
            ]
        ]


radioButtonGroupInnerStatue : List Shape2D -> Position -> Maybe Shape2D -> Html Msg
radioButtonGroupInnerStatue selectedShapes position selectedShape =
    let
        shapes =
            [ Circle, Square, Triangle ]

        messageHandler =
            SelectionInside position

        radioButtonClass =
            toString position ++ "2dShapes-radio-button"
    in
    radioButtonGroup <|
        List.map
            (\shape ->
                let
                    isSelected =
                        Maybe.withDefault False <|
                            Maybe.map (\s -> s == shape) selectedShape

                    isDisabled =
                        List.member shape selectedShapes
                in
                radioButton isSelected isDisabled position (messageHandler shape) radioButtonClass (toString2D shape)
            )
            shapes


radioButtonGroupOuterStatue : List Shape3D -> Position -> Maybe Shape2D -> Maybe Shape3D -> List Shape3D -> Html Msg
radioButtonGroupOuterStatue selectedOutsideShapes position selectedShapeInside selectedShapeOutside shapes =
    let
        messageHandler =
            SelectionOutside position

        radioButtonClass =
            toString position ++ "3dShapes-radio-button"
    in
    radioButtonGroup <|
        List.map
            (\shape ->
                let
                    isSelected =
                        Maybe.withDefault False <|
                            Maybe.map (\s -> s == shape) selectedShapeOutside

                    isDisabled =
                        (Maybe.withDefault False <|
                            Maybe.map (\s -> isIllegalShapeToStart s shape) selectedShapeInside
                        )
                            || checkLimts selectedOutsideShapes shape
                in
                radioButton isSelected isDisabled position (messageHandler shape) radioButtonClass (toString3D shape)
            )
            shapes


checkLimts : List Shape3D -> Shape3D -> Bool
checkLimts shapes selection =
    case selection of
        Sphere ->
            numberOfCircles shapes == maxNumberOf2DShapes

        Cube ->
            numberOfSquares shapes == maxNumberOf2DShapes

        Pyramid ->
            numberOfTriangles shapes == maxNumberOf2DShapes

        Cone ->
            numberOfTriangles shapes == maxNumberOf2DShapes || numberOfCircles shapes == maxNumberOf2DShapes

        Cylinder ->
            numberOfCircles shapes == maxNumberOf2DShapes || numberOfSquares shapes == maxNumberOf2DShapes

        Prism ->
            numberOfTriangles shapes == maxNumberOf2DShapes || numberOfSquares shapes == maxNumberOf2DShapes


renderStatue : List Shape2D -> List Shape3D -> Position -> StatueSelection -> Html Msg
renderStatue selectedInsideShapes selectedOutsideShapes position statueSelections =
    div
        [ css
            [ Tw.text_color Theme.white
            , Tw.bg_color Theme.zinc_700
            , Tw.px_3
            , Tw.py_2
            , Tw.border_solid
            , Tw.border_2
            , Tw.rounded
            , Tw.border_color Theme.slate_400
            ]
        ]
        [ h2 [] [ toString position |> text ]
        , div
            [ css [ Tw.flex ] ]
            [ div
                [ css [ Tw.py_2 ] ]
                [ Html.p [ css [ Tw.my_1, Tw.text_xl ] ] [ text "Inside Shape" ]
                , radioButtonGroupInnerStatue selectedInsideShapes position statueSelections.insideShape
                ]
            ]
        , hr [] []
        , div
            [ css [ Tw.flex ] ]
            [ div
                [ css [ Tw.py_2 ] ]
                [ Html.p [ css [ Tw.my_1, Tw.text_xl ] ] [ text "Outside Shape" ]
                , radioButtonGroupOuterStatue selectedOutsideShapes position statueSelections.insideShape statueSelections.outsideShape [ Sphere, Cube, Pyramid ]
                , radioButtonGroupOuterStatue selectedOutsideShapes position statueSelections.insideShape statueSelections.outsideShape [ Prism, Cone, Cylinder ]
                ]
            ]
        ]
