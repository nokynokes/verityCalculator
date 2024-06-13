module CalculatorTests exposing (..)

import Expect exposing (..)
import Test exposing (..)
import Maybe exposing (..)

import Shapes exposing(Shape2D(..), Shape3D(..))
import Calculator exposing (dissectShapes)

calculatorSuite : Test
calculatorSuite = 
    describe "Dissecting two shapes"
        [ describe "Circle"
            [ test "should swap with Triangle" <|
                \_ ->
                    case dissectShapes (Circle, Sphere) (Triangle, Prism) of
                        Just (Cone, Cylinder) -> Expect.pass
                        _ -> Expect.fail "should be Cone and Cylinder"
            , test "should swap with Square" <|
                \_ -> 
                    case dissectShapes (Circle, Sphere) (Square, Prism) of
                        Just (Cylinder, Cone) -> Expect.pass
                        _ -> Expect.fail "should be Cylinder and Cone"
            , test "should not be able to swap" <|
                \_ ->
                    case dissectShapes (Circle, Prism) (Triangle, Sphere) of
                        Nothing -> Expect.pass
                        _ -> Expect.fail "should not be able to create any new shapes"
            ]
        , describe "Sqaure"
            [ test "should swap with Circle" <|
                \_ ->
                    case dissectShapes (Square, Cube) (Circle, Sphere) of
                        Just (Cylinder, Cylinder) -> Expect.pass
                        _ -> Expect.fail "should both be Cylinder"
            , test "should swap with Triangle" <|
                \_ -> 
                    case dissectShapes (Square, Cylinder) (Triangle, Cone) of
                        Just (Cone, Cylinder) -> Expect.pass
                        _ -> Expect.fail "should be Cone and Cylinder"
            , test "should not be able to swap" <|
                \_ -> 
                    case dissectShapes (Square, Cone) (Circle, Prism) of
                        Nothing -> Expect.pass
                        _ -> Expect.fail "should not be able to create any new shapes"
            ]
        , describe "Triangle"
            [ test "should swap with Circle" <|
                \_ ->
                    case dissectShapes (Triangle, Pyramid) (Circle, Sphere) of
                        Just (Cone, Cone) -> Expect.pass
                        _ -> Expect.fail "should both be Cone"
            , test "should swap with Square" <|
                \_ -> 
                    case dissectShapes (Triangle, Prism) (Square, Cylinder) of
                        Just (Cube, Cone) -> Expect.pass
                        _ -> Expect.fail "should be Cube and Cone"
            , test "should not be able to swap" <|
                \_ -> 
                    case dissectShapes (Triangle, Cylinder) (Circle, Prism) of
                        Nothing -> Expect.pass
                        _ -> Expect.fail "should not be able to create any new shapes"
            ]
        ]