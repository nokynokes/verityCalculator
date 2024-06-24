module View.Steps exposing (..)

import Calculator exposing (dissectShapes)
import Css exposing (Style)
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
    [ Tw.flex, Tw.flex_row, Tw.bg_gradient_to_b, Tw.from_color Theme.emerald_400, Tw.to_color Theme.emerald_800 ]


stepTwoBackGround : List Style
stepTwoBackGround =
    [ Tw.flex, Tw.flex_row, Tw.bg_gradient_to_b, Tw.from_color Theme.blue_400, Tw.to_color Theme.blue_800 ]


stepForStatue : Int -> StatueDissect -> Html Msg
stepForStatue stepNumber dissect =
    let
        statueName =
            toString dissect.statueAfterDissect.position

        dissectShape =
            toString2D dissect.shapeToDissect

        finishedShape =
            toString3D dissect.statueAfterDissect.outsideShape
    in
    Html.div
        [ css [ Tw.basis_1over3, Tw.text_color Theme.white, Tw.text_4xl ] ]
        [ h1 [] [ (String.fromInt >> text) stepNumber ]
        , h3 [] [ text statueName ]
        , div
            []
            [ text <| "-" ++ dissectShape ]
        , h4 [] [ text "After dissect:" ]
        , div
            []
            [ text finishedShape ]
        , if isComplete dissect.statueAfterDissect then
            div
                []
                [ text <| statueName ++ " statue is now complete!" ]

          else
            div [] []
        ]


renderStep : Step -> Int -> List (Html Msg)
renderStep ( statue1, statue2 ) stepNumber =
    let
        render =
            stepForStatue stepNumber

        leftStatue =
            case ( statue1.statueAfterDissect.position, statue2.statueAfterDissect.position ) of
                ( Left, _ ) ->
                    render statue1

                ( _, Left ) ->
                    render statue2

                _ ->
                    Html.div [ css [ Tw.basis_1over3 ] ] []

        middleStatue =
            case ( statue1.statueAfterDissect.position, statue2.statueAfterDissect.position ) of
                ( Middle, _ ) ->
                    render statue1

                ( _, Middle ) ->
                    render statue2

                _ ->
                    Html.div [ css [ Tw.basis_1over3 ] ] []

        rightStatue =
            case ( statue1.statueAfterDissect.position, statue2.statueAfterDissect.position ) of
                ( Right, _ ) ->
                    render statue1

                ( _, Right ) ->
                    render statue2

                _ ->
                    Html.div [ css [ Tw.basis_1over3 ] ] []
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
                            stepOneBackGround

                container =
                    renderStep step stepNumber
                        |> div [ css backgroundGradient ]
            in
            container :: renderSteps_ (stepNumber + 1) tail

        [] ->
            []


renderSteps : List Step -> List (Html Msg)
renderSteps steps =
    renderSteps_ 1 steps
