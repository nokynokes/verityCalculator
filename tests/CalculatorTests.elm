module CalculatorTests exposing (..)

import Calculator exposing (dissectShapes, orderToSolve)
import Expect exposing (..)
import Shapes exposing (Shape2D(..), Shape3D(..))
import Statues.Internal exposing (Position(..))
import Test exposing (..)


dissectShapesTests : Test
dissectShapesTests =
    describe "Dissecting two shapes"
        [ describe "Circle"
            [ test "should swap with Triangle" <|
                \_ ->
                    case dissectShapes ( Circle, Sphere ) ( Triangle, Prism ) of
                        Just ( Cone, Cylinder ) ->
                            Expect.pass

                        _ ->
                            Expect.fail "should be Cone and Cylinder"
            , test "should swap with Square" <|
                \_ ->
                    case dissectShapes ( Circle, Cone ) ( Square, Prism ) of
                        Just ( Prism, Cone ) ->
                            Expect.pass

                        _ ->
                            Expect.fail "should be Cylinder and Cone"
            , test "should not be able to swap" <|
                \_ ->
                    case dissectShapes ( Circle, Prism ) ( Triangle, Sphere ) of
                        Nothing ->
                            Expect.pass

                        _ ->
                            Expect.fail "should not be able to create any new shapes"
            ]
        , describe "Sqaure"
            [ test "should swap with Circle" <|
                \_ ->
                    case dissectShapes ( Square, Cube ) ( Circle, Sphere ) of
                        Just ( Cylinder, Cylinder ) ->
                            Expect.pass

                        _ ->
                            Expect.fail "should both be Cylinder"
            , test "should swap with Triangle" <|
                \_ ->
                    case dissectShapes ( Square, Cylinder ) ( Triangle, Cone ) of
                        Just ( Cone, Cylinder ) ->
                            Expect.pass

                        _ ->
                            Expect.fail "should be Cone and Cylinder"
            , test "should not be able to swap" <|
                \_ ->
                    case dissectShapes ( Square, Cone ) ( Circle, Prism ) of
                        Nothing ->
                            Expect.pass

                        _ ->
                            Expect.fail "should not be able to create any new shapes"
            ]
        , describe "Triangle"
            [ test "should swap with Circle" <|
                \_ ->
                    case dissectShapes ( Triangle, Pyramid ) ( Circle, Sphere ) of
                        Just ( Cone, Cone ) ->
                            Expect.pass

                        _ ->
                            Expect.fail "should both be Cone"
            , test "should swap with Square" <|
                \_ ->
                    case dissectShapes ( Triangle, Prism ) ( Square, Cylinder ) of
                        Just ( Cube, Cone ) ->
                            Expect.pass

                        _ ->
                            Expect.fail "should be Cube and Cone"
            , test "should not be able to swap" <|
                \_ ->
                    case dissectShapes ( Triangle, Cylinder ) ( Circle, Prism ) of
                        Nothing ->
                            Expect.pass

                        _ ->
                            Expect.fail "should not be able to create any new shapes"
            ]
        ]


orderToSolveTests : Test
orderToSolveTests =
    describe "Reorder list of statues in order of what to solve first"
        [ test "should re order to M, R, L" <|
            \_ ->
                let
                    order =
                        [ { insideShape = Triangle 
                          , outsideShape = Prism
                          , position = Right
                          }
                        , { insideShape = Square
                          , outsideShape = Prism
                          , position = Middle
                          }
                        , { insideShape = Circle
                          , outsideShape = Sphere
                          , position = Left
                          }
                        ]
                            |> orderToSolve
                            |> List.map (\s -> s.position)
                in
                case order of
                    [ Middle, Right, Left ] ->
                        Expect.pass

                    _ ->
                        Expect.fail "Order should be Middle, Right, Left"
        , test "should re order to L, M, R" <|
            \_ ->
                let
                    order =
                        [ { insideShape = Triangle
                          , outsideShape = Sphere
                          , position = Left
                          }
                        , { insideShape = Circle
                          , outsideShape = Cube
                          , position = Middle
                          }
                        , { insideShape = Square
                          , outsideShape = Pyramid
                          , position = Right
                          }
                        ]
                            |> orderToSolve
                            |> List.map (\s -> s.position)
                in
                case order of
                    [ Left, Middle, Right ] ->
                        Expect.pass

                    _ ->
                        Expect.fail "Order should be Left, Middle, Right"
        , test "should re order to L, R, M" <|
            \_ ->
                let
                    order =
                        [ { insideShape = Triangle
                          , outsideShape = Cone
                          , position = Right
                          }
                        , { insideShape = Square
                          , outsideShape = Cube
                          , position = Middle
                          }
                        , { insideShape = Circle
                          , outsideShape = Cone
                          , position = Left
                          }
                        ]
                            |> orderToSolve
                            |> List.map (\s -> s.position)
                in
                case order of
                    [ Left, Right, Middle ] ->
                        Expect.pass

                    _ ->
                        Expect.fail "Order should be Left, Right, Middle"
        , test "should re order to L, M, R when number of steps are the same (2)" <|
            \_ ->
                let 
                    order =
                        [ { insideShape = Triangle
                          , outsideShape = Pyramid
                          , position = Middle
                          }
                        , { insideShape = Square
                          , outsideShape = Cube
                          , position = Right
                          }
                        , { insideShape = Circle
                          , outsideShape = Sphere
                          , position = Left
                          }
                        ]
                            |> orderToSolve
                            |> List.map (\s -> s.position)
                in
                    case order of
                        [Left, Middle, Right] ->
                            Expect.pass
                        _ -> Expect.fail "Order should be Left, Mid, Right"

        , test "should re order to L, M, R when number of steps are the same (1)" <|
            \_ ->
                let 
                    order =
                        [ { insideShape = Square
                          , outsideShape = Cylinder
                          , position = Middle
                          }
                        , { insideShape = Triangle
                          , outsideShape = Prism
                          , position = Right
                          }
                        , { insideShape = Circle
                          , outsideShape = Cone
                          , position = Left
                          }
                        ]
                            |> orderToSolve
                            |> List.map (\s -> s.position)
                in
                    case order of
                        [Left, Middle, Right] ->
                            Expect.pass
                        _ -> Expect.fail "Order should be Left, Mid, Right"
        ]
