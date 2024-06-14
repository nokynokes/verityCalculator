module Steps.Internal exposing (Step, generateStep)

import Calculator exposing (dissectShapes)
import Shapes exposing (Shape2D, hasShapes, removeForDoubleShapes, shapesMissing)
import Statues exposing (Statue)


type alias StatueDissect =
    { statueAfterDissect : Statue
    , shapeToDissect : Shape2D
    }


type alias Step =
    ( StatueDissect, StatueDissect )


generateStep : Statue -> Statue -> Maybe Step
generateStep s1 s2 =
    shapesMissing s1.insideShape s1.outsideShape
        |> Maybe.andThen
            (\missingShapes ->
                if not (hasShapes missingShapes s2.outsideShape) then
                    Nothing

                else
                    let
                        shapeToDissect1 =
                            Maybe.withDefault s1.insideShape (removeForDoubleShapes s1.outsideShape)

                        shapeToDissect2 =
                            Maybe.withDefault s2.insideShape (removeForDoubleShapes s2.outsideShape)
                    in
                    dissectShapes ( shapeToDissect1, s1.outsideShape ) ( shapeToDissect2, s2.outsideShape )
                        |> Maybe.map2 Tuple.pair (Just ( shapeToDissect1, shapeToDissect2 ))
            )
        |> Maybe.andThen
            (\( shapesToDissect, newShapes ) ->
                let
                    ( shape1, shape2 ) =
                        newShapes

                    ( shapeDissect1, shapeDissect2 ) =
                        shapesToDissect

                    firstStep =
                        { statueAfterDissect = { s1 | outsideShape = shape1 }, shapeToDissect = shapeDissect1 }

                    secondStep =
                        { statueAfterDissect = { s2 | outsideShape = shape2 }, shapeToDissect = shapeDissect2 }
                in
                Just ( firstStep, secondStep )
            )
