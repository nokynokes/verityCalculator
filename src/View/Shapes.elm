module View.Shapes exposing (circle)

import Svg.Styled as Svg exposing (Svg)
import Svg.Styled.Attributes exposing (cx, cy, r)


circle : Svg msg
circle =
    Svg.circle [ cx "60", cy "60", r "50" ] []
