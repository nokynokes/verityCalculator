module Steps exposing (..)
import Statues exposing (Statue)
import Shapes exposing (Shape2D)
type alias StatueDissect = 
    { statue: Statue
    , shapeToDissect: Shape2D
    }

type alias Step = (StatueDissect, StatueDissect)