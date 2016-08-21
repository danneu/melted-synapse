

module Waypoint exposing (..)


-- 1st
import Vector exposing (Vector)
import Action exposing (Action)


type alias Waypoint =
  { position : Vector
  , queue : List Action
  }


empty : Vector -> Waypoint
empty position =
  { position = position
  , queue = []
  }
