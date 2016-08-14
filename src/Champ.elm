

module Champ exposing (..)


-- Elm
import Svg exposing (..)
import Svg.Attributes
import Svg.Events
-- 1st
import Waypoint exposing (Waypoint)


type alias Champ =
  { position : (Int, Int)  -- x, y
  , waypoints : List Waypoint
  }


-- view : Champ -> Svg msg
-- view champ =
--   Svg.g
--   []
--   []
