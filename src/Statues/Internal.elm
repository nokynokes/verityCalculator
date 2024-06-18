module Statues.Internal exposing (Position(..), toString)


type Position
    = Left
    | Middle
    | Right


toString : Position -> String
toString pos =
    case pos of
        Left ->
            "Left"

        Middle ->
            "Middle"

        Right ->
            "Right"
