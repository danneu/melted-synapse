

module ChampDetail exposing (..)


-- Elm
import Html exposing (Html)
import Html.Events
-- 1st
import Champ exposing (Champ)
import Waypoint exposing (Waypoint)
import Class


-- MODEL


type alias Model =
  Champ


init : Champ -> Model
init champ =
  champ


-- UPDATE


type OutMsg
  = SelectWaypoint Waypoint


type Msg
  -- When user clicks on one of the waypoints from the champ detail list,
  -- switch to waypoint detail
  = ClickWaypointNav Waypoint


update : Msg -> Model -> (Model, OutMsg)
update msg model =
  case msg of
    ClickWaypointNav waypoint ->
      (model, SelectWaypoint waypoint)


-- VIEW


viewWaypoint : Waypoint -> Html Msg
viewWaypoint waypoint =
  let
    (x, y) = waypoint.position
  in
    Html.li
    []
    [ Html.button
      [ Html.Events.onClick (ClickWaypointNav waypoint) ]
      [ Html.text ("Waypoint (" ++ toString x ++ ", " ++ toString y ++ ")")
      ]
    ]



view : Model -> Html Msg
view champ =
  Html.div
  []
  [ Html.h2
    []
    [ Html.text (Class.toIcon champ.class ++ " " ++ champ.name)
    ]
  , Html.p
    []
    [ Html.text
        ("Status: " ++ Champ.statusToEmoji champ.status ++
         " " ++ Champ.statusToSimpleName champ.status
        )
    ]
  -- List champ's waypoints for easy navigation
  , Html.h3 [] [ Html.text "Waypoints" ]
  , if List.isEmpty champ.waypoints then
      Html.p [] [ Html.text "No Waypoints" ]
    else
      Html.ol
      []
      (List.map viewWaypoint champ.waypoints)
  , Html.p [] [ Html.text "Add a waypoint by double-clicking on a tile." ]
  , Html.p [] [ Html.text "Bug: Can't place waypoints on occupied tiles." ]
  ]
