

module Util exposing (..)


-- For when you know that Maybe is always Just
forceUnwrap : Maybe a -> a
forceUnwrap maybe =
  case maybe of
    Nothing ->
      Debug.crash "Impossible"
    Just value ->
      value


-- In our game, 0 is up. 90 is right, 180 is down. 270 is left. (CW)
-- On the render stage, 0 is right, 90 is up, 180 is left, 270 is down. (CCW)
-- Need to rotate gameAngle
toRenderAngle : Float -> Float
toRenderAngle gameAngle =
  (radians gameAngle) - (degrees 90)
