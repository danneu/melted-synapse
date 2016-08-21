

module Waypoint exposing (..)


-- 1st
import Vector exposing (Vector)
import Action exposing (Action)


type alias Waypoint =
  { position : Vector
  , actions : List Action
  }


empty : Vector -> Waypoint
empty position =
  { position = position
  , actions = []
  }


make : Vector -> List Action -> Waypoint
make position actions =
  { position = position
  , actions = actions
  }
