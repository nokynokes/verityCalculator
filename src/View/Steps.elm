module View.Steps exposing (..)

import Css exposing (Style, px)
import Css.Media as Media
import Html.Styled as Html exposing (Html, div, h1, h2, h3, h4, text)
import Html.Styled.Attributes as Html exposing (checked, css, step)
import Msg exposing (Msg)
import Shapes exposing (toString2D, toString3D)
import Statues exposing (isComplete)
import Statues.Internal exposing (Position(..), toString)
import Steps.Internal exposing (StatueDissect, Step)
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw


stepOneBackGround : List Style
stepOneBackGround =
    [ Tw.bg_gradient_to_b, Tw.from_color Theme.black, Tw.to_color Theme.orange_900 ]


stepTwoBackGround : List Style
stepTwoBackGround =
    [ Tw.bg_gradient_to_b, Tw.from_color Theme.black, Tw.to_color Theme.blue_900 ]


stepThreeBackGround : List Style
stepThreeBackGround =
    [ Tw.bg_gradient_to_b, Tw.from_color Theme.black, Tw.to_color Theme.green_900 ]


stepForStatue : StatueDissect -> Html Msg
stepForStatue dissect =
    let
        statueName =
            toString dissect.statueAfterDissect.position

        dissectShape =
            toString2D dissect.shapeToDissect
    in
    Html.div
        [ css [ Tw.basis_1over3, Tw.text_color Theme.white ] ]
        [ div [ css [ Tw.text_3xl, Tw.pb_3 ] ] [ text <| "On the " ++ String.toLower statueName ++ " statue:" ]
        , div
            [ css [ Tw.text_xl, Tw.py_3 ] ]
            [ text <| "Dissect a " ++ String.toLower dissectShape ]
        , if isComplete dissect.statueAfterDissect then
            div
                [ css [ Tw.text_3xl, Tw.py_3 ] ]
                [ text <| statueName ++ " statue is now complete!" ]

          else
            div [] []
        ]


renderStep : Step -> List (Html Msg)
renderStep ( statue1, statue2 ) =
    let
        emptyDiv =
            Html.div [ css [ Tw.basis_1over3, Media.withMedia [ Media.all [ Media.maxWidth (px 2169) ] ] [ Tw.hidden ] ] ] []

        leftStatue =
            case ( statue1.statueAfterDissect.position, statue2.statueAfterDissect.position ) of
                ( Left, _ ) ->
                    stepForStatue statue1

                ( _, Left ) ->
                    stepForStatue statue2

                _ ->
                    emptyDiv

        middleStatue =
            case ( statue1.statueAfterDissect.position, statue2.statueAfterDissect.position ) of
                ( Middle, _ ) ->
                    stepForStatue statue1

                ( _, Middle ) ->
                    stepForStatue statue2

                _ ->
                    emptyDiv

        rightStatue =
            case ( statue1.statueAfterDissect.position, statue2.statueAfterDissect.position ) of
                ( Right, _ ) ->
                    stepForStatue statue1

                ( _, Right ) ->
                    stepForStatue statue2

                _ ->
                    emptyDiv
    in
    [ leftStatue, middleStatue, rightStatue ]


renderSteps_ : Int -> List Step -> List (Html Msg)
renderSteps_ stepNumber steps =
    case steps of
        step :: tail ->
            let
                backgroundGradient =
                    case stepNumber of
                        1 ->
                            stepOneBackGround

                        2 ->
                            stepTwoBackGround

                        _ ->
                            stepThreeBackGround

                container =
                    div
                        [ css backgroundGradient ]
                        [ h1 [ css [ Tw.text_color Theme.white, Tw.px_10 ] ] [ "Step " ++ String.fromInt stepNumber |> text ]
                        , div [ css [ Tw.flex, Tw.flex_row, Tw.px_10, Bp.xxl [ Tw.gap_80 ], Bp.xxl [ Tw.justify_around ], Bp.xl [ Tw.justify_around ], Bp.lg [ Tw.justify_around ], Tw.justify_between ] ] (renderStep step)
                        ]
            in
            container :: renderSteps_ (stepNumber + 1) tail

        [] ->
            []


renderSteps : List Step -> List (Html Msg)
renderSteps steps =
    renderSteps_ 1 steps
