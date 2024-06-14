module Calculator.Internal exposing (compareStatues)

import Shapes exposing (Shape2D(..), Shape3D(..))
import Statues exposing (Statue)
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