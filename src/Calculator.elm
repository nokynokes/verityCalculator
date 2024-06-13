module Calculator exposing (..)

import Shapes exposing (Shape2D, Shape3D, combine, subtract)
import Shapes exposing (Shape2D(..))
import Shapes exposing (Shape3D(..))

type alias DissectPair = (Shape2D, Shape3D)

dissectShapes : DissectPair -> DissectPair -> (Shape3D, Shape3D)
dissectShapes (dissect1, shape1) (dissect2, shape2) = 
    let
        dissectResult1 = subtract dissect1 shape1
        dissectResult2 = subtract dissect2 shape2
    in
        (combine dissectResult1 dissect2, combine dissectResult2 dissect1)


isComplete : Shape2D -> Shape3D -> Bool
isComplete insideShape outsideShape = 
    case (Circle, Prism) -> True
    case (Square, Cone) -> True
    case (Triangle, Cylinder) -> True
    case _ -> False