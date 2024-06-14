module Main exposing (main)

import Browser
import Html.Events exposing (onClick)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw


main =
    Browser.sandbox { init = 0, update = update, view = view >> toUnstyled }


type Msg
    = Increment
    | Decrement


update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1


view _ =
    main_
        [ css
            [ Tw.bg_color Theme.zinc_900
            , Bp.lg [ Tw.px_40 ]
            , Bp.md [ Tw.px_20 ]
            , Bp.sm [ Tw.px_10 ]
            , Tw.scroll_smooth
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
            [ css [ Tw.flex, Tw.flex_row, Tw.min_h_screen ] ]
            []
        ]
