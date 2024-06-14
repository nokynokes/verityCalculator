module Statues exposing (..)
import Shapes exposing (Shape2D(..), Shape3D(..))

type Position = Left | Middle | Right

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