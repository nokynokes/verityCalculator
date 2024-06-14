module Calculator exposing (..)

import Calculator.Internal exposing (compareStatues)
import List exposing (sortWith)
import Maybe exposing (..)
import Shapes exposing (Shape2D(..), Shape3D(..), combine, subtract)
import Statues exposing (Statue)


type alias DissectPair =
    ( Shape2D, Shape3D )


dissectShapes : DissectPair -> DissectPair -> Maybe ( Shape3D, Shape3D )
dissectShapes ( dissect1, shape1 ) ( dissect2, shape2 ) =
    Maybe.map2 Tuple.pair (subtract dissect1 shape1) (subtract dissect2 shape2)
        |> Maybe.andThen
            (\( result1, result2 ) ->
                Just ( combine result1 dissect2, combine result2 dissect1 )
            )


orderToSolve : List Statue -> List Statue
orderToSolve statues =
    sortWith compareStatues statues
