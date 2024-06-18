module View.Statues exposing (renderStatue)

import Css
import Css.Global
import Html.Styled as Html exposing (Html, div, h2, text)
import Html.Styled.Attributes as Html exposing (css)
import Statues.Internal exposing (Position(..), toString)
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw


radioButton : String -> Html msg
radioButton name =
    Html.div
        [ Html.css
            [ Tw.flex
            , Tw.flex_col
            ]
        , Html.class "radio-button"
        ]
        [ Html.input
            [ Html.type_ "radio"
            , Html.id "myCustomRadio"
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
            , Html.for "myCustomRadioid"
            , Html.css
                [ Css.displayFlex
                , Css.alignItems Css.center
                , Css.justifyContent Css.center
                , Css.padding <| Css.rem 0.8
                , Css.cursor Css.pointer
                ]
            ]
            [ text name ]
        ]


radioButtonGroup : Html msg
radioButtonGroup =
    Html.div
        [ Html.css
            [ Css.displayFlex
            , Css.border3 (Css.px 1) Css.solid (Css.hex "efefef")
            , Css.borderRadius <| Css.px 4
            , Css.Global.descendants
                [ Css.Global.selector ".radio-button:not(:last-of-type)"
                    [ Css.borderRight3 (Css.px 1) Css.solid (Css.hex "efefef") ]
                ]
            ]
        ]
    <|
        List.map
            radioButton
            [ "Circle", "Square", "Triangle" ]


renderStatue : Position -> Html msg
renderStatue position =
    div [ css [ Tw.text_color Theme.white ] ]
        [ h2 [] [ toString position |> text ]
        , radioButtonGroup
        ]
