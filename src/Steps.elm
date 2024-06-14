module Steps exposing (..)
import Statues exposing (Statue, isComplete)
import Shapes exposing (Shape2D, hasShapes, shapesMissing, removeForDoubleShapes)
import Calculator exposing (dissectShapes)
type alias StatueDissect = 
    { statueAfterDissect: Statue
    , shapeToDissect: Shape2D
    }

type alias Step = (StatueDissect, StatueDissect)

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

generateStep : Statue -> Statue -> Maybe Step
generateStep s1 s2 =
    shapesMissing s1.insideShape s1.outsideShape
        |> Maybe.andThen 
            (\missingShapes -> 
                if not (hasShapes missingShapes s2.outsideShape) then
                    Nothing
                else
                    let 
                        shapeToDissect1 = Maybe.withDefault s1.insideShape (removeForDoubleShapes s1.outsideShape)
                        shapeToDissect2 = Maybe.withDefault s2.insideShape (removeForDoubleShapes s2.outsideShape)
                    in
                        dissectShapes (shapeToDissect1, s1.outsideShape) (shapeToDissect2, s2.outsideShape)
                            |> Maybe.map2 Tuple.pair (Just (shapeToDissect1, shapeToDissect2))
            )
        |> Maybe.andThen
            (\(shapesToDissect, newShapes) -> 
                let
                    (shape1, shape2) = newShapes
                    (shapeDissect1, shapeDissect2) = shapesToDissect
                    firstStep = { statueAfterDissect = { s1 | outsideShape = shape1  } , shapeToDissect = shapeDissect1 }
                    secondStep = { statueAfterDissect = { s2 | outsideShape = shape2 } , shapeToDissect = shapeDissect2 }
                in 
                    Just (firstStep, secondStep)
            
            )


    