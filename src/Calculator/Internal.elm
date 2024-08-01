module Calculator.Internal exposing (compareStatues)

import Shapes exposing (Shape2D(..), Shape3D(..))
import Statues exposing (Statue)
import Statues.Internal exposing (Position)
import Statues.Internal exposing (Position(..))


numberOfStepsToCompelete : Shape2D -> Shape3D -> Int
numberOfStepsToCompelete insideShape outsideShape =
    case ( insideShape, outsideShape ) of
        ( Circle, Sphere ) ->
            2

        ( Square, Cube ) ->
            2

        ( Triangle, Pyramid ) ->
            2

        _ ->
            1

positionOrder : Position -> Int
positionOrder pos = 
    case pos of
        Left -> 1
        Middle -> 2
        Right -> 3


compareStatues : Statue -> Statue -> Order
compareStatues s1 s2 =
    let
        steps1 =
            numberOfStepsToCompelete s1.insideShape s1.outsideShape

        steps2 =
            numberOfStepsToCompelete s2.insideShape s2.outsideShape

        position1 = positionOrder s1.position
        position2 = positionOrder s2.position

        stepsCompare = compare steps1 steps2
    in
    case stepsCompare of
        EQ -> compare position1 position2
        _ -> stepsCompare
