

module Constants exposing (..)


roundLengthInSeconds : Float
roundLengthInSeconds =
  4.0


ticksPerRound : Int
ticksPerRound =
  round (roundLengthInSeconds * 60)


tilesize : Float
tilesize =
  64
