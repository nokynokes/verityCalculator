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
    div
        [ css
            [ Tw.bg_color Theme.zinc_900
            , Tw.min_h_screen
            , Tw.text_5xl
            , Tw.text_color Theme.amber_100
            , Tw.px_20
            ]
        ]
        [ text "Hello World!" ]
