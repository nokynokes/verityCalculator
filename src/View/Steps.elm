module View.Steps exposing (renderSteps)

import Css exposing (Style)
import Html.Styled as Html exposing (Html, div, h1, h2, hr, text)
import Html.Styled.Attributes as Html exposing (css, step)
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

stepForStatue : StatueDissect -> Html Msg
stepForStatue dissect =
    let
        statueName =
            toString dissect.statueAfterDissect.position

        dissectShape =
            toString2D dissect.shapeToDissect
    in
    Html.div
        [ css [ Tw.text_color Theme.white ] ]
        [ div [ css [ Tw.text_3xl, Tw.pb_3 ] ] [ text <| "On the " ++ String.toLower statueName ++ " statue:" ]
        , div
            [ css [ Tw.text_xl, Tw.py_3 ] ]
            [ Html.p [] [ text <| "Dissect a " ++ String.toLower dissectShape ] ]
        ]

afterDissect : StatueDissect -> Html Msg
afterDissect dissect =
    let
        statueName =
            toString dissect.statueAfterDissect.position

        afterDissectShape =
            toString3D dissect.statueAfterDissect.outsideShape
    in
    Html.div
        [ css [ Tw.text_color Theme.white ] ]
        [ div [ css [ Tw.text_3xl, Tw.pb_3 ] ] [ text <| "On the " ++ String.toLower statueName ++ " statue:" ]
        , div
            [ css [ Tw.text_xl, Tw.py_3 ] ]
            [ Html.p [] [ text <| "There should be a " ++ afterDissectShape ]
            , if isComplete dissect.statueAfterDissect.insideShape dissect.statueAfterDissect.outsideShape then
                 Html.p []
                         [ text <| statueName ++ " is done!"
                         ]

              else
                Html.p [] [text ""]
            ]
        ]



renderShapesAfterStep : Step -> List (Html Msg)
renderShapesAfterStep ( statue1, statue2 ) =
    [afterDissect statue1, afterDissect statue2]


renderStep : Step -> List (Html Msg)
renderStep ( statue1, statue2 ) =
    [stepForStatue statue1, stepForStatue statue2]

renderSteps_ : Int -> Step -> Html Msg
renderSteps_ stepNumber step = 
    div
        [ css stepOneBackGround ]
        [ h1 [ css [ Tw.text_color Theme.white ] ] [ "Step " ++ String.fromInt (stepNumber + 1) |> text ]
        , div [ css [ Tw.flex, Tw.flex_row, Tw.gap_64, Bp.xxl [ Tw.justify_around ], Bp.xl [ Tw.justify_around ], Bp.lg [ Tw.justify_around ], Tw.justify_between ] ] (renderStep step)
        , hr [] []
        , h2 [] [ text "After dissection: " ]
        , div [ css [ Tw.flex, Tw.flex_row, Tw.gap_64, Bp.xxl [ Tw.justify_around ], Bp.xl [ Tw.justify_around ], Bp.lg [ Tw.justify_around ], Tw.justify_between ] ] (renderShapesAfterStep step)
        ]



renderSteps : List Step -> List (Html Msg)
renderSteps =
    List.indexedMap renderSteps_
