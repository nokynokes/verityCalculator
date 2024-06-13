module CalculatorTests exposing (..)

import Expect exposing (..)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Maybe

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
        ]