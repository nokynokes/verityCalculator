module Statues exposing (Statue, isComplete)

import Shapes exposing (Shape2D(..), Shape3D(..))
import Statues.Internal exposing (Position)


type alias Statue =
    { position : Position
    , insideShape : Shape2D
    , outsideShape : Shape3D
    }


isComplete : Shape2D -> Shape3D -> Bool
isComplete inside outside =
    case ( inside, outside ) of
        ( Circle, Prism ) ->
            True

        ( Square, Cone ) ->
            True

        ( Triangle, Cylinder ) ->
            True

        _ ->
            False
