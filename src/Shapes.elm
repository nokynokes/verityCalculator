module Shapes exposing (..)

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

subtract : Shape2D -> Shape3D -> Shape2D
subtract dissect shape = 
    case (dissect, shape) of
        (Circle, Sphere) -> Circle
        (Square, Cube) -> Square
        (Triangle, Pyramid) -> Triangle
        (Circle, Cylinder) -> Square
        (Square, Cylinder) -> Circle
        (Circle, Cone) -> Triangle
        (Triangle, Cone) -> Circle
        (Square, Prism) -> Triangle
        (Triangle, Prism) -> Square
        _ -> dissect

-- type Shape3D = Combine Shape2D Shape2D

-- sphere : Shape3D
-- sphere = Combine Circle Circle

-- cube : Shape3D
-- cube = Combine Square Square

-- pyramid : Shape3D
-- pyramid = Combine Triangle Triangle

-- cylinder : Shape3D
-- cylinder = Combine Circle Square

-- cone : Shape3D
-- cone = Combine Circle Triangle

-- prism : Shape3D
-- prism = Combine Square Triangle

-- func = Combine Circle