module View.Steps exposing (..)

import Css exposing (Style, px)
import Css.Media as Media
import Html.Styled as Html exposing (Html, div, h1, h2, h3, h4, hr, text)
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
    [ Tw.bg_color Theme.zinc_700, Tw.border_solid, Tw.border_2, Tw.border_color Theme.slate_400, Tw.rounded, Tw.px_10 ]


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

        -- , if isComplete dissect.statueAfterDissect then
        --     div
        --         [ css [ Tw.text_3xl, Tw.py_3 ] ]
        --         [ text <| statueName ++ " statue is now complete!" ]
        --   else
        --     div [] []
        ]


renderShapesAfterStep : Step -> Html Msg
renderShapesAfterStep ( statue1, statue2 ) =
    let
        shape1 =
            div []
                [ Html.p []
                    [ text <| toString statue1.statueAfterDissect.position ++ " --> " ++ toString3D statue1.statueAfterDissect.outsideShape ]
                , if isComplete statue1.statueAfterDissect then
                    Html.p []
                        [ text <| toString statue1.statueAfterDissect.position ++ " is done!"
                        ]

                  else
                    text ""
                ]

        shape2 =
            div []
                [ Html.p
                    []
                    [ text <| toString statue2.statueAfterDissect.position ++ " --> " ++ toString3D statue2.statueAfterDissect.outsideShape
                    ]
                , if isComplete statue2.statueAfterDissect then
                    Html.p []
                        [ text <| toString statue2.statueAfterDissect.position ++ " is done!"
                        ]

                  else
                    text ""
                ]
    in
    div
        []
        [ h2 [] [ text "Outside Shapes after dissection: " ]
        , div [ css [ Tw.text_2xl ] ] [ shape1, shape2 ]
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
                container =
                    div
                        [ css stepOneBackGround ]
                        [ h1 [ css [ Tw.text_color Theme.white ] ] [ "Step " ++ String.fromInt stepNumber |> text ]
                        , div [ css [ Tw.flex, Tw.flex_row, Bp.xxl [ Tw.gap_80 ], Bp.xxl [ Tw.justify_around ], Bp.xl [ Tw.justify_around ], Bp.lg [ Tw.justify_around ], Tw.justify_between ] ] (renderStep step)
                        , hr [] []
                        , renderShapesAfterStep step
                        ]
            in
            container :: renderSteps_ (stepNumber + 1) tail

        [] ->
            []


renderSteps : List Step -> List (Html Msg)
renderSteps steps =
    renderSteps_ 1 steps
