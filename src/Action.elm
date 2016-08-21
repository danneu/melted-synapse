

module Action exposing (..)


-- Elm
import Time exposing (Time)


type alias Angle =
  Float


-- TODO: Figure out how to leverage the type system here better.
--       For example, is it possible for a Warrior's waypoint to only
--       enqueue Warrior actions?
type Action
  -- GENERAL
  = Wait Time
  -- WARRIOR
  --| Cleave Angle
  --| WarCry Angle
  -- RANGER
  --| Snipe Angle
