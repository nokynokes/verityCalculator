module Steps exposing (generateSteps)

import Statues exposing (Statue, isComplete)
import Steps.Internal exposing (Step, generateStep)


type StepBuild 
    = FirstTwo 
    | FirstAndLast 

buildSteps : Statue -> Statue -> Statue -> StepBuild -> Step -> List Statue
buildSteps s1 s2 s3 stepBuild step = 
    let
        (step1, step2) = step
        statueComplete = isComplete step1.statueAfterDissect.insideShape step1.statueAfterDissect.outsideShape
    in
        case (statueComplete, stepBuild) of
            (True, FirstTwo) -> [ { s2 | outsideShape = step2.statueAfterDissect.outsideShape }, s3 ]
            (True, FirstAndLast) -> [ s2, { s3 | outsideShape = step2.statueAfterDissect.outsideShape } ]
            (False, FirstTwo) -> [ { s1 | outsideShape = step1.statueAfterDissect.outsideShape }, { s2 | outsideShape = step2.statueAfterDissect.outsideShape }, s3 ]
            (False, FirstAndLast) -> [ { s1 | outsideShape = step1.statueAfterDissect.outsideShape }, s2, { s3 | outsideShape = step2.statueAfterDissect.outsideShape } ]


generateSteps : List Statue -> List Step
generateSteps statues =
    case statues of
        [ s1, s2, s3 ] ->
            let 
                handleStep = \stepBuild step -> Just <| step :: generateSteps (buildSteps s1 s2 s3 stepBuild step)
            in
            generateStep s1 s2
                |> Maybe.andThen (handleStep FirstTwo)
                |> Maybe.withDefault 
                    ( generateStep s1 s3
                        |> Maybe.andThen (handleStep FirstAndLast)
                        |> Maybe.withDefault []
                    )
        [ s1, s2 ] ->
            generateStep s1 s2
                |> Maybe.andThen (List.singleton >> Just)
                |> Maybe.withDefault []

        _ ->
            []
