module Statues exposing (Statue, isComplete)
import Statues.Internal exposing (Position)
import Shapes exposing (Shape2D(..), Shape3D(..))

type alias Statue = 
    { position: Position
    , insideShape: Shape2D
    , outsideShape: Shape3D
    }

isComplete : Statue -> Bool
isComplete statue =
    case (statue.insideShape, statue.outsideShape) of 
        (Circle, Prism) -> True
        (Square, Cone) -> True
        (Triangle, Cylinder) -> True
        _ -> False