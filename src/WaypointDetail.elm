

module WaypointDetail exposing (..)


-- Elm
import Html exposing (Html)
import Html.Events
import Html.Attributes
import List.Extra
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
  | RemoveAction Int -- Int is index
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
    RemoveAction idx ->
      let
        waypoint' =
          { waypoint
              | actions = List.Extra.removeAt idx waypoint.actions
          }
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


viewAction : Int -> Action -> Html Msg
viewAction idx action =
  Html.li
  []
  [ Html.button
    [ Html.Events.onClick (RemoveAction idx)
    , Html.Attributes.class "btn btn-danger btn-xs"
    , Html.Attributes.style [ ("margin-right", "5px") ]
    ]
    [ Html.text "X" ]
  , Action.toHtml action
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
      (List.indexedMap viewAction waypoint.actions)
  , Html.p [] [ Html.text "Add an action" ]
  , Html.p
    [ Html.Attributes.style [ ("font-style", "italic") ] ]
    [ Html.text "TODO: Implement charge angle UI instead of cardinal direction buttons" ]
  , Html.ul
    []
    [ Html.li
      []
      [ Html.text "🚀 Charge: "
      , Html.button
        [ Html.Events.onClick (AddAction (Action.Charge (degrees 180))) ]
        [ Html.text "←" ]
      , Html.button
        [ Html.Events.onClick (AddAction (Action.Charge (degrees 270))) ]
        [ Html.text "↑" ]
      , Html.button
        [ Html.Events.onClick (AddAction (Action.Charge (degrees 90))) ]
        [ Html.text "↓" ]
      , Html.button
        [ Html.Events.onClick (AddAction (Action.Charge (degrees 0))) ]
        [ Html.text "→" ]
      ]
    ]
  ]
