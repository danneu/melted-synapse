

module WaypointDetail exposing (..)


-- Elm
import Html exposing (Html)
import Html.Events
-- 1st
import Waypoint exposing (Waypoint)
import Action exposing (Action)
import Vector
import Util


-- MODEL


-- TODO: Need to change this to champ so I can do stuff
--       like Class.toEmoji champ.class. Failed attempt to
--       just take only what I'd probably need.
type alias Model =
  { champName : String
  , waypoint : Waypoint
  }


init : String -> Waypoint -> Model
init champName waypoint =
  Model champName waypoint


-- UPDATE


type OutMsg
  = UpdateWaypoint String Waypoint


type Msg
  = AddAction Action
  --| RemoveAction Int Waypoint -- Int is index
  | ClearActions


-- FIXME: Factor out the repetition of updating the model and the upstream
update : Msg -> Model -> (Model, OutMsg)
update msg ({champName, waypoint} as model) =
  case msg of
    AddAction action ->
      let
        waypoint' = { waypoint | actions = List.append waypoint.actions [action] }
      in
        ( { model | waypoint = waypoint' }
        , UpdateWaypoint champName  waypoint'
        )
    ClearActions ->
      let
        waypoint' = { waypoint | actions = [] }
      in
        ( { model | waypoint = waypoint' }
        , UpdateWaypoint champName  waypoint'
        )


-- VIEW


viewAction : Action -> Html Msg
viewAction action =
  Html.li
  []
  [ Html.text (toString action)
  ]


view : Model -> Html Msg
view {champName, waypoint} =
  Html.div
  []
  [ Html.h2
    []
    [ Html.text
        ("Waypoint " ++ (Vector.toString waypoint.position))
    ]
  , Html.p
    []
    [ Html.text ("Champ: " ++ champName)
    ]
  , Html.h3 [] [ Html.text "Actions" ]
  , if List.isEmpty waypoint.actions then
      Html.text "No Actions"
    else
      Html.ol
      []
      (List.map viewAction waypoint.actions)
  , Html.p [] [ Html.text "Add an action" ]
  , Html.ul
    []
    [ Html.li
      []
      [ Html.button
        [ Html.Events.onClick
            (AddAction (Action.Charge (Util.toRenderAngle (degrees 0))))
        ]
        [ Html.text "Charge up" ]
      ]
    , Html.li
      []
      [ Html.button
        [ Html.Events.onClick
            (AddAction (Action.Charge (Util.toRenderAngle (degrees 90))))
        ]
        [ Html.text "Charge right" ]
      ]
    , Html.li
      []
      [ Html.button
        [ Html.Events.onClick
            (AddAction (Action.Charge (Util.toRenderAngle (degrees 180))))
        ]
        [ Html.text "Charge down" ]
      ]
    , Html.li
      []
      [ Html.button
        [ Html.Events.onClick
            (AddAction (Action.Charge (Util.toRenderAngle (degrees 270))))
        ]
        [ Html.text "Charge left" ]
      ]
    ]
  ]
