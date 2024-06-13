module Calculator exposing (..)

import Shapes exposing (Shape2D(..), Shape3D(..), combine, subtract)

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
    case (insideShape, outsideShape) of 
        (Circle, Prism) -> True
        (Square, Cone) -> True
        (Triangle, Cylinder) -> True
        _ -> False