module Calculator exposing (..)

import Shapes exposing (Shape2D(..), Shape3D(..), combine, subtract)
import Maybe exposing (..)
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


isComplete : Shape2D -> Shape3D -> Bool
isComplete insideShape outsideShape =
    case (insideShape, outsideShape) of 
        (Circle, Prism) -> True
        (Square, Cone) -> True
        (Triangle, Cylinder) -> True
        _ -> False