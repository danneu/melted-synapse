

module WaypointDetail exposing (..)


-- Elm
import Html exposing (Html)
import Html.Events
import Html.Attributes
import List.Extra
-- 1st
import Waypoint exposing (Waypoint)
import Champ
import Class
import Action exposing (Action)
import Vector


-- MODEL


-- TODO: Need to change this to champ so I can do stuff
--       like Class.toEmoji champ.class. Failed attempt to
--       just take only what I'd probably need.
type alias Model =
  { champ : Champ.Champ
  , waypoint : Waypoint
  }


init : Champ.Champ -> Waypoint -> Model
init champ waypoint =
  Model champ waypoint


-- UPDATE


type OutMsg
  = UpdateWaypoint String Waypoint


type Msg
  = AddAction Action
  | RemoveAction Int -- Int is index
  | ClearActions


-- FIXME: Factor out the repetition of updating the model and the upstream
update : Msg -> Model -> (Model, OutMsg)
update msg ({champ, waypoint} as model) =
  case msg of
    AddAction action ->
      let
        waypoint' = { waypoint | actions = List.append waypoint.actions [action] }
      in
        ( { model | waypoint = waypoint' }
        , UpdateWaypoint champ.name  waypoint'
        )
    RemoveAction idx ->
      let
        waypoint' =
          { waypoint
              | actions = List.Extra.removeAt idx waypoint.actions
          }
      in
        ( { model | waypoint = waypoint' }
        , UpdateWaypoint champ.name waypoint'
        )
    ClearActions ->
      let
        waypoint' = { waypoint | actions = [] }
      in
        ( { model | waypoint = waypoint' }
        , UpdateWaypoint champ.name  waypoint'
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
view {champ, waypoint} =
  Html.div
  []
  [ Html.h2
    []
    [ Html.text
        ("Waypoint " ++ (Vector.toString waypoint.position))
    ]
  , Html.p
    []
    [ Html.text ("Champ: " ++ champ.name)
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
    [ Html.text "TODO: Implement charge angle UI instead of cardinal direction buttons, and implement slider for wait duration." ]
  , Html.ul
    []
    [ -- GENERAL
      Html.li
      []
      [ Html.text "⌛ Wait: "
      , Html.br [] []
      , Html.button
        [ Html.Events.onClick (AddAction (Action.Wait (1, 30))) ]
        [ Html.text "0.5 sec" ]
      , Html.button
        [ Html.Events.onClick (AddAction (Action.Wait (1, 60))) ]
        [ Html.text "1 sec" ]
      , Html.button
        [ Html.Events.onClick (AddAction (Action.Wait (1, 90))) ]
        [ Html.text "1.5 sec" ]
      , Html.button
        [ Html.Events.onClick (AddAction (Action.Wait (1, 120))) ]
        [ Html.text "2 sec" ]
      ]
      -- WARRIOR
    , if champ.class /= Class.Warrior then
        Html.text ""
      else
        Html.li
        []
        [ Html.text "🚀 Charge: "
        , Html.br [] []
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
      -- RANGER
    , if champ.class /= Class.Ranger then
        Html.text ""
      else
        Html.li
        []
        [ Html.text "🎯 Snipe: "
        , Html.br [] []
        , Html.button
          [ Html.Events.onClick (AddAction (Action.Snipe (degrees 180) waypoint.position (1, 60))) ]
          [ Html.text "←" ]
        , Html.button
          [ Html.Events.onClick (AddAction (Action.Snipe (degrees 270) waypoint.position (1, 60))) ]
          [ Html.text "↑" ]
        , Html.button
          [ Html.Events.onClick (AddAction (Action.Snipe (degrees 90) waypoint.position (1, 60))) ]
          [ Html.text "↓" ]
        , Html.button
          [ Html.Events.onClick (AddAction (Action.Snipe (degrees 0) waypoint.position (1, 60))) ]
          [ Html.text "→" ]
        ]
    ]
  ]
