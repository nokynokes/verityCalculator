module View.Steps exposing (..)

import Css exposing (Style)
import Html.Styled as Html exposing (Html, div, h2, text)
import Html.Styled.Attributes as Html exposing (checked, css, step)
import Msg exposing (Msg)
import Shapes exposing (toString2D, toString3D)
import Statues.Internal exposing (Position(..), toString)
import Steps.Internal exposing (StatueDissect, Step)
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw


stepOneBackGround : List Style
stepOneBackGround =
    [ Tw.flex, Tw.flex_row, Tw.bg_gradient_to_b, Tw.from_color Theme.emerald_400, Tw.to_color Theme.emerald_800 ]


stepForStatue : StatueDissect -> Html Msg
stepForStatue dissect =
    Html.div
        [ css [ Tw.basis_1over3, Tw.text_color Theme.white, Tw.text_4xl ] ]
        [ (toString >> text) dissect.statueAfterDissect.position
        , (toString2D >> text) dissect.shapeToDissect
        , (toString3D >> text) dissect.statueAfterDissect.outsideShape
        ]


renderStep : Step -> List (Html Msg)
renderStep ( statue1, statue2 ) =
    let
        leftStatue =
            case ( statue1.statueAfterDissect.position, statue2.statueAfterDissect.position ) of
                ( Left, _ ) ->
                    stepForStatue statue1

                ( _, Left ) ->
                    stepForStatue statue2

                _ ->
                    Html.div [ css [ Tw.basis_1over3 ] ] []

        middleStatue =
            case ( statue1.statueAfterDissect.position, statue2.statueAfterDissect.position ) of
                ( Middle, _ ) ->
                    stepForStatue statue1

                ( _, Middle ) ->
                    stepForStatue statue2

                _ ->
                    Html.div [ css [ Tw.basis_1over3 ] ] []

        rightStatue =
            case ( statue1.statueAfterDissect.position, statue2.statueAfterDissect.position ) of
                ( Right, _ ) ->
                    stepForStatue statue1

                ( _, Right ) ->
                    stepForStatue statue2

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

                        _ ->
                            stepOneBackGround

                container =
                    renderStep step
                        |> div [ css backgroundGradient ]
            in
            container :: renderSteps_ (stepNumber + 1) tail

        [] ->
            []


renderSteps : List Step -> List (Html Msg)
renderSteps steps =
    renderSteps_ 1 steps
