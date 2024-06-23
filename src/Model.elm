module Model exposing (Model, StatueSelection, initModel)

import Shapes exposing (Shape2D(..), Shape3D)
import Steps.Internal exposing (Step)


type alias StatueSelection =
    { insideShape : Maybe Shape2D
    , outsideShape : Maybe Shape3D
    }


type alias Model =
    { leftStatueSelections : StatueSelection
    , middleStatueSelections : StatueSelection
    , rightStatueSelections : StatueSelection
    , steps : List Step
    }


emptySelection : StatueSelection
emptySelection =
    { insideShape = Nothing, outsideShape = Nothing }


initModel : Model
initModel =
    { leftStatueSelections = emptySelection, middleStatueSelections = emptySelection, rightStatueSelections = emptySelection, steps = [] }
