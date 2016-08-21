

module Action exposing (..)


-- Elm
import Time exposing (Time)


type alias Angle =
  Float


-- TODO: Figure out how to leverage the type system here better.
--       For example, is it possible for a Warrior's waypoint to only
--       enqueue Warrior actions?
-- TODO: Will there be General actions not specific to a class?
-- type Action
--   -- WARRIOR
--   = Charge Angle
--   | WarCry Angle
--   -- RANGER
--   | Snipe Angle

type Action
  = WarAct WarriorAction
  | RanAct RangerAction


type WarriorAction
  = Charge Angle
  | WarCry Angle
  | AutoAttack


type RangerAction
  = Snipe Angle
