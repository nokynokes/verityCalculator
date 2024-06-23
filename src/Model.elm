module Model exposing (Model)

import Statues exposing (Statue)
import Steps.Internal exposing (Step)


type alias Model =
    { leftStatue : Statue
    , middleStatue : Statue
    , rightStatue : Statue
    , steps : List Step
    }
