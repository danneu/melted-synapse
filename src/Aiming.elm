


module Aiming exposing (..)


import Svg
import Svg.Attributes
-- 1st
import Vector exposing (Vector)
import Action exposing (Action)


type Mode
  -- Aiming for a new action on a champ/waypoint
  = New
  -- Aiming for an existing action on a champ/waypoint
  | Edit Int -- Int is action idx


-- model.selection is the origin of the candidate (i.e. a champ or waypoint)
type Candidate -- TODO: Rename to .shape
  = Ray Float -- angle
  -- | Cone Angle ...
  -- | Point Vector Radius ...


type alias Aiming =
  { mode : Mode
  , candidate : Candidate
  , origin : Vector
  , action : Action
  , update : (Candidate, Action) -> Action
  }


-- AIMING


view : Vector -> Aiming -> Svg.Svg msg
view originPx {candidate, origin, action} =
  case candidate of
    Ray angle ->
      let
        (x1, y1) =
          originPx
        len =
          200
        -- line end
        (x2, y2) =
          ( x1 + len * cos angle
          , y1 + len * sin angle
          )
        -- line end + a lil more
        (x3, y3) =
          ( x1 + (len + (len / 10)) * cos angle
          , y1 + (len + (len / 10)) * sin angle
          )
      in
        Svg.g
        []
        [ Svg.line
          [ Svg.Attributes.x1 (toString x1)
          , Svg.Attributes.y1 (toString y1)
          , Svg.Attributes.x2 (toString x2)
          , Svg.Attributes.y2 (toString y2)
          , Svg.Attributes.stroke "red"
          , Svg.Attributes.strokeWidth "10"
          , Svg.Attributes.strokeOpacity "0.75"
          ]
          []
        , Svg.text'
          [ Svg.Attributes.x (toString (x1 + 10))
          , Svg.Attributes.y (toString (y1 - 10))
          , Svg.Attributes.fill "red"
          , Svg.Attributes.style "font-size: 24px"
          ]
          [ Svg.text "Click map to choose direction" ]
          -- Show action icon at tip of line
        , Svg.text'
          [ Svg.Attributes.x (toString (x3 - 10))
          , Svg.Attributes.y (toString (y3 + 10))
          , Svg.Attributes.style "font-size: 24px"
          ]
          [ Svg.text (Action.toIcon action)]
        ]
