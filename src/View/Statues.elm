module View.Statues exposing (renderStatue)

import Css exposing (hover, position)
import Css.Global
import Html.Styled as Html exposing (Html, div, h2, text)
import Html.Styled.Attributes as Html exposing (checked, css)
import Html.Styled.Events exposing (onCheck)
import Model exposing (StatueSelection)
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
            , Html.disabled isDisabled
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
                , hover [ Tw.text_color Theme.amber_100 ]
                ]
            ]
            [ text name ]
        ]


radioButtonGroup : String -> List (Html msg) -> Html msg
radioButtonGroup className =
    Html.div
        [ Html.css
            [ Tw.flex
            , Tw.py_1
            , Css.border3 (Css.px 1) Css.solid (Css.hex "efefef")
            , Css.borderRadius <| Css.px 4
            , Css.Global.descendants
                [ Css.Global.selector ("." ++ className ++ ":not(:last-of-type)")
                    [ Css.borderRight3 (Css.px 1) Css.solid (Css.hex "efefef") ]
                ]
            ]
        ]


radioButtonGroupInnerStatue : Position -> Maybe Shape2D -> Html Msg
radioButtonGroupInnerStatue position selectedShape =
    let
        shapes =
            [ Circle, Square, Triangle ]

        messageHandler =
            SelectionInside position

        radioButtonClass =
            toString position ++ "2dShapes-radio-button"
    in
    radioButtonGroup radioButtonClass <|
        List.map
            (\shape ->
                let
                    isSelected =
                        Maybe.withDefault False <|
                            Maybe.map (\s -> s == shape) selectedShape
                in
                radioButton isSelected False position (messageHandler shape) radioButtonClass (toString2D shape)
            )
            shapes


radioButtonGroupOuterStatue : Position -> Maybe Shape2D -> Maybe Shape3D -> Html Msg
radioButtonGroupOuterStatue position selectedShapeInside selectedShapeOutside =
    let
        shapes =
            [ Cube, Sphere, Pyramid, Prism, Cone, Cylinder ]

        messageHandler =
            SelectionOutside position

        radioButtonClass =
            toString position ++ "3dShapes-radio-button"
    in
    radioButtonGroup radioButtonClass <|
        List.map
            (\shape ->
                let
                    isSelected =
                        Maybe.withDefault False <|
                            Maybe.map (\s -> s == shape) selectedShapeOutside

                    isDisabled =
                        Maybe.withDefault False <|
                            Maybe.map (\s -> isIllegalShapeToStart s shape) selectedShapeInside
                in
                radioButton isSelected isDisabled position (messageHandler shape) radioButtonClass (toString3D shape)
            )
            shapes


renderStatue : Position -> StatueSelection -> Html Msg
renderStatue position statueSelections =
    div [ css [ Tw.text_color Theme.white ] ]
        [ h2 [] [ toString position |> text ]
        , div [ css [ Tw.flex ] ] [ div [ css [ Tw.py_2 ] ] [ text "Inner Statue Shape", radioButtonGroupInnerStatue position statueSelections.insideShape ] ]
        , div [ css [ Tw.flex ] ] [ div [ css [ Tw.py_2 ] ] [ text "Outer Statue Shape", radioButtonGroupOuterStatue position statueSelections.insideShape statueSelections.outsideShape ] ]
        ]
