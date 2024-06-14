module StepsTests exposing (..)

import Expect exposing (..)
import Test exposing (..)

import Shapes exposing(Shape2D(..), Shape3D(..))
import Statues.Internal exposing (Position(..))
import Statues exposing (Statue)
import Steps.Internal exposing (Step, generateStep)
import Steps exposing (generateSteps)

generateStatue : Position -> Shape2D -> Shape3D -> Statue
generateStatue pos shape2d shape3d = 
    { position = pos
    , insideShape = shape2d
    , outsideShape = shape3d
    }

generateSingleStepTests : Test
generateSingleStepTests =
    describe "Generate a single step btwn to statues"
        [   test "should swap Square with Circle when single shapes" <|
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
            , test "should swap Triangle with Circle when single shapes" <|
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
            , test "should swap Square with Circle" <|
                \_ -> 
                    let
                        statue1 = generateStatue Left Circle Cube
                        statue2 = generateStatue Right Square Pyramid
                    in
                        case generateStep statue1 statue2 of
                            Just steps -> 
                                let 
                                    (step1, step2) = steps
                                in
                                    case (step1.statueAfterDissect.position, step1.statueAfterDissect.outsideShape, step1.shapeToDissect) of
                                        (Left, Prism, Square) -> 
                                            case (step2.statueAfterDissect.position, step2.statueAfterDissect.outsideShape, step2.shapeToDissect) of
                                                (Right, Prism, Triangle) -> Expect.pass
                                                _ -> Expect.fail "It should have generated a step to subtract triangle from pyramid and create a Prism"
                                        _ -> Expect.fail "It should have generated a step to subtract square from cube and create a Prism"
                            _ -> Expect.fail "It should have generated one step"
            , test "should swap Square with Circle when inside shapes are same as double shapes" <|
                \_ -> 
                    let
                        statue1 = generateStatue Left Circle Sphere
                        statue2 = generateStatue Middle Square Cube
                    in
                        case generateStep statue1 statue2 of
                            Just steps -> 
                                let 
                                    (step1, step2) = steps
                                in
                                    case (step1.statueAfterDissect.position, step1.statueAfterDissect.outsideShape, step1.shapeToDissect) of
                                        (Left, Cylinder, Circle) -> 
                                            case (step2.statueAfterDissect.position, step2.statueAfterDissect.outsideShape, step2.shapeToDissect) of
                                                (Middle, Cylinder, Square) -> Expect.pass
                                                _ -> Expect.fail "It should have generated a step to subtract sqaure from cube and create a cylinder"
                                        _ -> Expect.fail "It should have generated a step to subtract circle from sphere and create a clyinder"
                            _ -> Expect.fail "It should have generated one step"
            ]
        
statuesOrdered1 : List Statue
statuesOrdered1 = 
    [   { insideShape = Circle, outsideShape = Sphere, position = Left }
    ,   { insideShape = Square, outsideShape = Prism, position = Middle }
    ,   { insideShape = Triangle, outsideShape = Prism, position = Right }
    ]

expectedSteps1 : List Step
expectedSteps1 = 
    [
        (
            { shapeToDissect = Square
            , statueAfterDissect = { insideShape = Square, outsideShape = Cone, position = Middle } 
            }
        ,   { shapeToDissect = Circle
            , statueAfterDissect = { insideShape = Circle, outsideShape = Cylinder, position = Left } 
            }
        )
        ,
        (
            { shapeToDissect = Triangle
            , statueAfterDissect = { insideShape = Triangle, outsideShape = Cylinder, position = Right } 
            }
        ,   { shapeToDissect = Circle
            , statueAfterDissect = { insideShape = Circle, outsideShape = Prism, position = Left } 
            }
        )
    ]
statuesOrdered2 : List Statue
statuesOrdered2 = 
    [   { insideShape = Circle, outsideShape = Sphere, position = Left }
    ,   { insideShape = Square, outsideShape = Cube, position = Middle }
    ,   { insideShape = Triangle, outsideShape = Pyramid, position = Right }
    ]

generateStepsTest : Test
generateStepsTest = 
    describe "Generate Steps btwn all three statues" 
        [   describe "in exactly two steps"
                [   test "should dissect btwn middle and left first then right and left"
                        (\_ -> 
                            let 
                                steps = generateSteps statuesOrdered1 
                            in
                                case steps of
                                    [_,_] -> Expect.pass
                                    _ -> Expect.fail "it should generate only 2 steps"
                        )

                ]
        ,   describe "in exactly three steps"
                [   test "should dissect btwn Left and Middle first then middle and right"
                        (\_ -> 
                            let
                                steps = generateSteps statuesOrdered2
                            in
                                case steps of
                                    [_,_,_] -> Expect.pass
                                    _ -> Expect.fail "it should generate only 3 steps"
                            
                        )
                ]

        ]