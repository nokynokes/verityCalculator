module Steps exposing (..)

import Steps.Internal exposing (Step, generateStep)
import Statues exposing (Statue, isComplete)


generateSteps : List Statue -> List Step
generateSteps statues = 
    case statues of
        [s1, s2, s3] -> 
            case generateStep s1 s2 of
                Just (step1, step2) ->
                    if isComplete step1.statueAfterDissect then
                        (step1, step2) :: generateSteps [{s2 | outsideShape = step2.statueAfterDissect.outsideShape }, s3]
                    else 
                        (step1, step2) ::  generateSteps [{ s1 | outsideShape = step1.statueAfterDissect.outsideShape }, {s2 | outsideShape = step2.statueAfterDissect.outsideShape }, s3]
                Nothing -> 
                    case generateStep s1 s3 of
                        Just (step1, step3) ->
                            if isComplete step1.statueAfterDissect then
                                (step1, step3) :: generateSteps [s2, {s3 | outsideShape = step3.statueAfterDissect.outsideShape}]
                            else 
                                (step1, step3) ::  generateSteps [{ s1 | outsideShape = step1.statueAfterDissect.outsideShape }, s2, {s3 | outsideShape = step3.statueAfterDissect.outsideShape}]
                        Nothing -> []
                        
        [s1, s2] -> 
            case generateStep s1 s2 of
                Just step -> [step]
                Nothing -> []
        _ -> []

    