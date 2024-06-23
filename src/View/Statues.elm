module View.Statues exposing (renderStatue)

import Css exposing (position, rad)
import Css.Global
import Html.Styled as Html exposing (Html, div, h2, text)
import Html.Styled.Attributes as Html exposing (checked, css)
import Html.Styled.Events exposing (onClick)
import Msg exposing (Msg(..))
import Shapes exposing (Shape2D(..), Shape3D(..), toString2D, toString3D)
import Statues.Internal exposing (Position(..), toString)
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw


radioButton : Position -> Msg -> String -> String -> Html Msg
radioButton position message className name =
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
            , onClick message
            , Html.css
                [ Tw.hidden
                , Css.checked
                    [ Css.Global.generalSiblings
                        [ Css.Global.selector ".radio-button__label"
                            [ Css.backgroundColor <| Css.hex "88c0d0" ]
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


radioButtonGroupInnerStatue : Position -> Html Msg
radioButtonGroupInnerStatue position =
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
            (\shape -> radioButton position (messageHandler shape) radioButtonClass (toString2D shape))
            shapes


radioButtonGroupOuterStatue : Position -> Html Msg
radioButtonGroupOuterStatue position =
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
            (\shape -> radioButton position (messageHandler shape) radioButtonClass (toString3D shape))
            shapes


renderStatue : Position -> Html Msg
renderStatue position =
    div [ css [ Tw.text_color Theme.white ] ]
        [ h2 [] [ toString position |> text ]
        , div [ css [ Tw.flex ] ] [ div [ css [ Tw.py_2 ] ] [ text "Inner Statue Shape", radioButtonGroupInnerStatue position ] ]
        , div [ css [ Tw.flex ] ] [ div [ css [ Tw.py_2 ] ] [ text "Outer Statue Shape", radioButtonGroupOuterStatue position ] ]
        ]
