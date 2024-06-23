module Shapes exposing
    ( Shape2D(..)
    , Shape3D(..)
    , combine
    , hasShapes
    , removeForDoubleShapes
    , shapesMissing
    , subtract
    , toString2D
    , toString3D
    )


type Shape2D
    = Circle
    | Square
    | Triangle


type Shape3D
    = Sphere
    | Cube
    | Pyramid
    | Cylinder
    | Cone
    | Prism


combine : Shape2D -> Shape2D -> Shape3D
combine shape1 shape2 =
    case ( shape1, shape2 ) of
        ( Circle, Circle ) ->
            Sphere

        ( Square, Square ) ->
            Cube

        ( Triangle, Triangle ) ->
            Pyramid

        ( Circle, Square ) ->
            Cylinder

        ( Square, Circle ) ->
            Cylinder

        ( Circle, Triangle ) ->
            Cone

        ( Triangle, Circle ) ->
            Cone

        ( Square, Triangle ) ->
            Prism

        ( Triangle, Square ) ->
            Prism


subtract : Shape2D -> Shape3D -> Maybe Shape2D
subtract dissectShape shape =
    case ( dissectShape, shape ) of
        ( Circle, Sphere ) ->
            Just Circle

        ( Square, Cube ) ->
            Just Square

        ( Triangle, Pyramid ) ->
            Just Triangle

        ( Circle, Cylinder ) ->
            Just Square

        ( Square, Cylinder ) ->
            Just Circle

        ( Circle, Cone ) ->
            Just Triangle

        ( Triangle, Cone ) ->
            Just Circle

        ( Square, Prism ) ->
            Just Triangle

        ( Triangle, Prism ) ->
            Just Square

        _ ->
            Nothing


shapesMissing : Shape2D -> Shape3D -> Maybe (List Shape2D)
shapesMissing insideShape outsideShape =
    case ( insideShape, outsideShape ) of
        ( Square, Prism ) ->
            Just [ Circle ]

        ( Square, Cylinder ) ->
            Just [ Triangle ]

        ( Square, Cube ) ->
            Just [ Triangle, Circle ]

        ( Square, Sphere ) ->
            Just [ Triangle ]

        ( Square, Pyramid ) ->
            Just [ Circle ]

        ( Triangle, Prism ) ->
            Just [ Circle ]

        ( Triangle, Cone ) ->
            Just [ Square ]

        ( Triangle, Cube ) ->
            Just [ Circle ]

        ( Triangle, Sphere ) ->
            Just [ Square ]

        ( Triangle, Pyramid ) ->
            Just [ Circle, Square ]

        ( Circle, Cylinder ) ->
            Just [ Triangle ]

        ( Circle, Cone ) ->
            Just [ Square ]

        ( Circle, Cube ) ->
            Just [ Triangle ]

        ( Circle, Sphere ) ->
            Just [ Square, Triangle ]

        ( Circle, Pyramid ) ->
            Just [ Square ]

        _ ->
            Nothing


hasShapes : List Shape2D -> Shape3D -> Bool
hasShapes shapes outsideShape =
    List.map (hasShape outsideShape) shapes
        |> List.foldl (||) False


hasShape : Shape3D -> Shape2D -> Bool
hasShape outsideShape insideShape =
    case subtract insideShape outsideShape of
        Just _ ->
            True

        _ ->
            False


removeForDoubleShapes : Shape3D -> Maybe Shape2D
removeForDoubleShapes outsideShape =
    case outsideShape of
        Sphere ->
            Just Circle

        Cube ->
            Just Square

        Pyramid ->
            Just Triangle

        _ ->
            Nothing


toString2D : Shape2D -> String
toString2D shape =
    case shape of
        Circle ->
            "Circle"

        Square ->
            "Square"

        Triangle ->
            "Triangle"


toString3D : Shape3D -> String
toString3D shape =
    case shape of
        Sphere ->
            "Sphere"

        Pyramid ->
            "Pyramid"

        Cube ->
            "Cube"

        Prism ->
            "Prism"

        Cone ->
            "Cone"

        Cylinder ->
            "Cylinder"
