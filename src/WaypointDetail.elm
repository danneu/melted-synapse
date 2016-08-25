

module WaypointDetail exposing (..)


-- Elm
import Html exposing (Html)
import Html.Events
import Html.Attributes
import List.Extra
-- 1st
import Waypoint exposing (Waypoint)
import Champ exposing (Champ)
import Class
import Action exposing (Action)
import Vector exposing (Vector)


-- FIXME: Messy
--
-- I ended up combining ChampDetail and WaypointDetail into this module
-- after changing the system to allow Champs to have actions enqueued on them.
-- Couldn't think of a nice way to avoid duplicate logic for managing
-- actions in ChampDetail vs WaypointDetail, so I changed this module's
-- Module to accept a (Maybe Waypoint) so this module could be overloaded
-- to support Champ selection.


-- MODEL


type alias Model =
  { champ : Champ
  , waypoint : Maybe Waypoint
  }


init : Champ -> Maybe Waypoint -> Model
init champ maybeWaypoint =
  Model champ maybeWaypoint


-- UPDATE


type OutMsg
  -- champName and waypoint position allow us to find a unqiue waypoint
  = UpdateWaypointActions String Vector (List Action)
  | UpdateChampActions String (List Action)


type Msg
  = AddAction Action
  | RemoveAction Int -- Int is index
  | ClearActions


-- FIXME: NASTY
-- FIXME: NASTY
-- FIXME: NASTY
-- FIXME: NASTY
-- FIXME: NASTY
update : Msg -> Model -> (Model, OutMsg)
update msg ({champ} as model) =
  case msg of
    AddAction action ->
      case model.waypoint of
        Nothing ->
          let
            champ = model.champ
            actions' = List.append champ.actions [action]
            champ' = { champ | actions = actions' }
          in
            ( { model | champ = champ' }
            , UpdateChampActions champ.name actions'
            )
        Just waypoint ->
          let
            actions' = List.append waypoint.actions [action]
            waypoint' = Just { waypoint | actions = actions' }
          in
            ( { model | waypoint = waypoint' }
            , UpdateWaypointActions champ.name waypoint.position actions'
            )
    RemoveAction idx ->
      case model.waypoint of
        Nothing ->
          let
            champ = model.champ
            actions' = List.Extra.removeAt idx champ.actions
            champ' = { champ | actions = actions' }
          in
            ( { model | champ = champ' }
            , UpdateChampActions champ.name actions'
            )
        Just waypoint ->
          let
            actions' = List.Extra.removeAt idx waypoint.actions
            waypoint' = Just { waypoint | actions = actions' }
          in
            ( { model | waypoint = waypoint' }
            , UpdateWaypointActions champ.name waypoint.position actions'
            )
    ClearActions ->
      case model.waypoint of
        Nothing ->
          let
            champ = model.champ
            actions' = []
            champ' = { champ | actions = actions' }
          in
            ( { model | champ = champ' }
            , UpdateChampActions champ.name actions'
            )
        Just waypoint ->
          let
            actions' = []
            waypoint' = Just { waypoint | actions = actions' }
          in
            ( { model | waypoint = waypoint' }
            , UpdateWaypointActions champ.name waypoint.position actions'
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
view ({champ} as model) =
  let
    (actions, position) =
      case model.waypoint of
        Nothing ->
          (champ.actions, champ.position)
        Just waypoint ->
          (waypoint.actions, waypoint.position)
  in
  Html.div
  []
  [ Html.h2
    []
    [ Html.text
        ( case model.waypoint of
            Nothing ->
              champ.name
            Just waypoint ->
              ("Waypoint " ++ (Vector.toString waypoint.position))
        )
    ]
  , if model.waypoint == Nothing then
      Html.text ""
    else
      Html.p
      []
      [ Html.text ("Champ: " ++ champ.name)
      ]
  , Html.h3 [] [ Html.text "Actions" ]
  , if List.isEmpty actions then
      Html.text "-- No Actions --"
    else
      Html.ol
      []
      (List.indexedMap viewAction actions)
  , Html.h4 [] [ Html.text "Add an action" ]
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
          [ Html.Events.onClick (AddAction (Action.Snipe (degrees 180) position (1, 60))) ]
          [ Html.text "←" ]
        , Html.button
          [ Html.Events.onClick (AddAction (Action.Snipe (degrees 270) position (1, 60))) ]
          [ Html.text "↑" ]
        , Html.button
          [ Html.Events.onClick (AddAction (Action.Snipe (degrees 90) position (1, 60))) ]
          [ Html.text "↓" ]
        , Html.button
          [ Html.Events.onClick (AddAction (Action.Snipe (degrees 0) position (1, 60))) ]
          [ Html.text "→" ]
        ]
    ]
  , Html.p
    [ Html.Attributes.style [ ("font-style", "italic") ] ]
    [ Html.text "TODO: Implement charge angle UI instead of cardinal direction buttons, and implement slider for wait duration." ]
  ]