module View.Statues exposing (renderStatue)

import Css exposing (position, rad)
import Css.Global
import Html.Styled as Html exposing (Html, div, h2, text)
import Html.Styled.Attributes as Html exposing (css)
import Statues.Internal exposing (Position(..), toString)
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw


radioButton : Position -> String -> String -> Html msg
radioButton position className name =
    let
        id =
            toString position ++ name
    in
    Html.div
        [ Html.css
            [ Tw.flex
            , Tw.flex_col
            ]
        , Html.class className
        ]
        [ Html.input
            [ Html.type_ "radio"
            , Html.name className
            , Html.id id
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


radioButtonGroup : Position -> Html msg
radioButtonGroup position =
    let
        radioButtonClass =
            toString position ++ "radio-button"
    in
    Html.div
        [ Html.css
            [ Css.displayFlex
            , Css.border3 (Css.px 1) Css.solid (Css.hex "efefef")
            , Css.borderRadius <| Css.px 4
            , Css.Global.descendants
                [ Css.Global.selector ("." ++ radioButtonClass ++ ":not(:last-of-type)")
                    [ Css.borderRight3 (Css.px 1) Css.solid (Css.hex "efefef") ]
                ]
            ]
        ]
    <|
        List.map
            (radioButton position radioButtonClass)
            [ "Circle", "Square", "Triangle" ]


renderStatue : Position -> Html msg
renderStatue position =
    div [ css [ Tw.text_color Theme.white ] ]
        [ h2 [] [ toString position |> text ]
        , radioButtonGroup position
        ]
