

module Action exposing (..)


-- 1st
import Util


--
-- Actions are things that a champ can enqueue at a Waypoint
--


type alias Angle =
  Float


-- Starts at (1, tickDuration), ends at (tickDuration, tickDuration)
type alias Duration =
  (Int, Int)


-- TODO: Will there be General actions not specific to a class?
type Action
  -- WARRIOR
  = Charge Angle
  --| WarCry Angle Duration
  -- RANGER
  | Snipe Angle Duration



-- User-friendly name of the action
toString : Action -> String
toString action =
  case action of
    Charge angle ->
      let
        degrees =
          round (Util.toDegrees angle)
      in
        "Charge " ++ Basics.toString degrees ++ "°"
    Snipe angle _ ->
      let
        degrees =
          round (Util.toDegrees angle)
      in
        "Snipe " ++ Basics.toString degrees ++ "°"
