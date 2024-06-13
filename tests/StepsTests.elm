module StepsTests exposing (..)

import Expect exposing (..)
import Test exposing (..)

import Shapes exposing(Shape2D(..), Shape3D(..))
import Statues exposing (Statue, Position(..))
import Steps exposing (generateStep)

generateStatue : Position -> Shape2D -> Shape3D -> Statue
generateStatue pos shape2d shape3d = 
    { position = pos
    , insideShape = shape2d
    , outsideShape = shape3d
    }

generateStepsTest : Test
generateStepsTest = 
    describe "Generate Steps"
        [ describe "between two statues"
            [ test "should swap Square with Circle" <|
                \_ -> 
                    let
                        statue1 = generateStatue Middle Square Prism
                        statue2 = generateStatue Left Circle Cone
                    in
                        case generateStep statue1 statue2 of
                            Just steps -> 
                                let 
                                    (step1, step2) = steps
                                in
                                    case (step1.statueAfterDissect.position, step1.statueAfterDissect.outsideShape, step1.shapeToDissect) of
                                        (Middle, Cone, Square) -> 
                                            case (step2.statueAfterDissect.position, step2.statueAfterDissect.outsideShape, step2.shapeToDissect) of
                                                (Left, Prism, Circle) -> Expect.pass
                                                _ -> Expect.fail "It should have generated a step to subtract cricle from cone and create a Prism"
                                        _ -> Expect.fail "It should have generated a step to subtract square from prism and create a cone"
                            _ -> Expect.fail "It should have generated one step"
            , test "should swap Triangle with Circle" <|
                \_ -> 
                    let
                        statue1 = generateStatue Right Triangle Prism
                        statue2 = generateStatue Left Circle Sphere
                    in
                        case generateStep statue1 statue2 of
                            Just steps -> 
                                let 
                                    (step1, step2) = steps
                                in
                                    case (step1.statueAfterDissect.position, step1.statueAfterDissect.outsideShape, step1.shapeToDissect) of
                                        (Right, Cylinder, Triangle) -> 
                                            case (step2.statueAfterDissect.position, step2.statueAfterDissect.outsideShape, step2.shapeToDissect) of
                                                (Left, Cone, Circle) -> Expect.pass
                                                _ -> Expect.fail "It should have generated a step to subtract circle from sphere and create a cone"
                                        _ -> Expect.fail "It should have generated a step to subtract triangle from prism and create a clyinder"
                            _ -> Expect.fail "It should have generated one step"
            , test "should not be able to generate a step" <|
                \_ -> 
                    let
                        statue1 = 
                            { position = Middle
                            , insideShape = Square
                            , outsideShape = Cone 
                            }
                        statue2 = 
                            { position = Left
                            , insideShape = Circle
                            , outsideShape = Cone
                            }
                    in
                        case generateStep statue1 statue2 of
                            Nothing -> Expect.pass
                            _ -> Expect.fail "It should not be able to subtract a square from a cone"
            ]
            
        ]