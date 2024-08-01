module Steps exposing (..)

import Statues exposing (Statue, isComplete)
import Steps.Internal exposing (Step, generateStep)


generateSteps : List Statue -> List Step
generateSteps statues =
    case statues of
        [ s1, s2, s3 ] ->
            generateStep s1 s2
                |> Maybe.andThen (\(step1, step2) -> 
                    if isComplete step1.statueAfterDissect.insideShape step1.statueAfterDissect.outsideShape then
                         Just <| ( step1, step2 ) :: generateSteps [ { s2 | outsideShape = step2.statueAfterDissect.outsideShape }, s3 ]

                    else
                        Just <| ( step1, step2 ) :: generateSteps [ { s1 | outsideShape = step1.statueAfterDissect.outsideShape }, { s2 | outsideShape = step2.statueAfterDissect.outsideShape }, s3 ]
                )
                |> Maybe.withDefault 
                    ( generateStep s1 s3
                        |> Maybe.andThen (\(step1, step3) -> 
                            if isComplete step1.statueAfterDissect.insideShape step1.statueAfterDissect.outsideShape then
                                 Just <| ( step1, step3 ) :: generateSteps [ s2, { s3 | outsideShape = step3.statueAfterDissect.outsideShape } ]

                             else
                                 Just <| ( step1, step3 ) :: generateSteps [ { s1 | outsideShape = step1.statueAfterDissect.outsideShape }, s2, { s3 | outsideShape = step3.statueAfterDissect.outsideShape } ]
                        )
                        |> Maybe.withDefault []
                    )
        [ s1, s2 ] ->
            generateStep s1 s2
                |> Maybe.andThen (List.singleton >> Just)
                |> Maybe.withDefault []

        _ ->
            []
