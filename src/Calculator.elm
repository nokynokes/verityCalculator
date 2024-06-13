module Calculator exposing (..)

import Shapes exposing (Shape2D(..), Shape3D(..), combine, subtract)
import Statues exposing (Statue)
import Maybe exposing (..)
import List exposing (sortWith)
type alias DissectPair = (Shape2D, Shape3D)

dissectShapes : DissectPair -> DissectPair -> Maybe (Shape3D, Shape3D)
dissectShapes (dissect1, shape1) (dissect2, shape2) = 
    let
        dissectResult1 = subtract dissect1 shape1
        dissectResult2 = subtract dissect2 shape2
    in
        case (dissectResult1, dissectResult2) of
            (Just result1, Just result2) -> Just (combine result1 dissect2, combine result2 dissect1)
            _ -> Nothing

numberOfStepsToCompelete : Shape2D -> Shape3D -> Int
numberOfStepsToCompelete insideShape outsideShape = 
    case (insideShape, outsideShape) of
        (Circle, Sphere) -> 2
        (Square, Cube) -> 2
        (Triangle, Pyramid) -> 2
        _ -> 1

compareStatues : Statue -> Statue -> Order
compareStatues s1 s2 =
    let 
        steps1 = numberOfStepsToCompelete s1.insideShape s1.outsideShape
        steps2 = numberOfStepsToCompelete s2.insideShape s2.outsideShape
    in
        compare steps1 steps2


orderToSolve : List Statue -> List Statue
orderToSolve statues = 
   sortWith compareStatues statues
    