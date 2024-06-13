module Steps exposing (..)
import Statues exposing (Statue)
import Shapes exposing (Shape2D)
import Calculator exposing (dissectShapes)
type alias StatueDissect = 
    { statueAfterDissect: Statue
    , shapeToDissect: Shape2D
    }

type alias Step = (StatueDissect, StatueDissect)

generateSteps : List Statue -> List Step
generateSteps statues = 
    case statues of
        [s1, s2, s3] -> []
        [s1, s2] -> 
            case generateStep s1 s2 of
                Just step -> [step]
                Nothing -> []
        _ -> []

generateStep : Statue -> Statue -> Maybe Step
generateStep s1 s2 =
    let 
        pair1 = (s1.insideShape, s1.outsideShape)
        pair2 = (s2.insideShape, s2.outsideShape)
    in
        dissectShapes pair1 pair2
            |> Maybe.andThen 
                (\(shape1, shape2) -> 
                    let
                        firstStep = { statueAfterDissect = { s1 | outsideShape = shape1  } , shapeToDissect = s1.insideShape }
                        secondStep = { statueAfterDissect = { s2 | outsideShape = shape2 } , shapeToDissect = s2.insideShape }
                    in 
                        Just (firstStep, secondStep)
                )


    