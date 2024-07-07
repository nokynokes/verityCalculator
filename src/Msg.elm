module Msg exposing (Msg(..))

import Shapes exposing (Shape2D, Shape3D)
import Statues.Internal exposing (Position(..))


type Msg
    = SelectionInside Position Shape2D
    | SelectionOutside Position Shape3D
    | ResetSelections
    | NoOp
