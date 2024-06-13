module Shapes exposing (..)
import Maybe exposing (..)
type Shape2D = Circle | Square | Triangle 
type Shape3D 
    = Sphere 
    | Cube
    | Pyramid
    | Cylinder
    | Cone
    | Prism

combine : Shape2D -> Shape2D -> Shape3D
combine shape1 shape2 = 
    case (shape1, shape2) of
        (Circle, Circle) -> Sphere
        (Square, Square) -> Cube
        (Triangle, Triangle) -> Pyramid 
        (Circle, Square) -> Cylinder
        (Square, Circle) -> Cylinder
        (Circle, Triangle) -> Cone
        (Triangle, Circle) -> Cone
        (Square, Triangle) -> Prism
        (Triangle, Square) -> Prism

subtract : Shape2D -> Shape3D -> Maybe Shape2D
subtract dissectShape shape = 
    case (dissectShape, shape) of
        (Circle, Sphere) -> Just Circle
        (Square, Cube) -> Just Square
        (Triangle, Pyramid) -> Just Triangle
        (Circle, Cylinder) -> Just Square
        (Square, Cylinder) -> Just Circle
        (Circle, Cone) -> Just Triangle
        (Triangle, Cone) -> Just Circle
        (Square, Prism) -> Just Triangle
        (Triangle, Prism) -> Just Square
        _ -> Nothing
        
isComplete : Shape2D -> Shape3D -> Bool
isComplete insideShape outsideShape =
    case (insideShape, outsideShape) of 
        (Circle, Prism) -> True
        (Square, Cone) -> True
        (Triangle, Cylinder) -> True
        _ -> False