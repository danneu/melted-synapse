

module Action exposing (..)


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
