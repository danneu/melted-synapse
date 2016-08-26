

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
import Aiming


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
  | UpdateAiming (Maybe Aiming.Aiming)


type Msg
  = AddAction Action
  | RemoveAction Int -- Int is index
  | ClearActions
  | StartAiming Float Action ((Aiming.Candidate, Action) -> Action) -- angle


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
    StartAiming angle action update ->
      let
        origin =
          model.waypoint
          |> Maybe.map .position
          |> Maybe.withDefault model.champ.position
        aiming =
          ( Just { mode = Aiming.New
                 , candidate = Aiming.Ray angle
                 , origin = origin
                 , action = action
                 , update = update
                 }
          )
      in
        (model, UpdateAiming aiming)


-- VIEW


viewAction : Int -> Action -> Html Msg
viewAction idx action =
  Html.li
  []
  [ Html.button
    [ Html.Events.onClick (RemoveAction idx)
    , Html.Attributes.class "btn btn-danger btn-xs"
    , Html.Attributes.style [ ("margin-right", "10px") ]
    ]
    [ Html.text "X" ]
  , Html.text ((Action.toIcon action) ++ " ")
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
    -- Show the owner champ if it's a waypoint
  , if model.waypoint == Nothing then
      Html.text ""
    else
      Html.p
      []
      [ Html.text ("Champ: " ++ champ.name)
      ]
    -- Show champ HP if it's a champ panel
  , if model.waypoint /= Nothing then
      Html.text ""
    else
      let
        (currHp, maxHp, _) = champ.hp
        widthStyle =
          (toString ((toFloat currHp / toFloat maxHp) * 100)) ++ "%"
          |> Debug.log "widthStyle"
      in
        Html.div
        [ Html.Attributes.class "progress" ]
        [ Html.div
          [ Html.Attributes.class "progress-bar"
          , Html.Attributes.style
              [ ("width", widthStyle)
                -- min-width so that the bar is wide enough to read even
                -- with low hp
              , ("min-width", "75px")
              ]
          ]
          [ Html.text (toString currHp ++ " / " ++ toString maxHp ++ " hp")
          ]
        ]
  , Html.h3
    []
    [ Html.text "Actions "
    , Html.button
      [ Html.Attributes.class "btn btn-danger btn-xs"
      , Html.Events.onClick ClearActions
      ]
      [ Html.text "Clear" ]
    ]
  , if List.isEmpty actions then
      Html.text "-- No Actions --"
    else
      Html.ol
      [ Html.Attributes.class "list-unstyled" ]
      (List.indexedMap viewAction actions)
  , Html.h4 [] [ Html.text "Add an action" ]
  , if List.length actions == 5 then
      Html.text "-- Cannot enqueue more than 5 actions --"
    else
      Html.ul
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
          [ Html.button
            [ let
                update = \ (candidate, action) ->
                  case action of
                    Action.Charge _ origin ->
                      case candidate of
                        Aiming.Ray angle ->
                          Action.Charge angle origin
                    _ ->
                      Debug.crash "Impossible"
              in
                Html.Events.onClick
                  (StartAiming 0 (Action.Charge 0 position) update)
            ]
            [ Html.text "🚀 Charge"
            ]
          ]
        -- RANGER
      , if champ.class /= Class.Ranger then
          Html.text ""
        else
          Html.li
          []
          [ Html.button
            [ let
                update = \ (candidate, prevAction) ->
                  case prevAction of
                    Action.Snipe _ vector duration ->
                      case candidate of
                        Aiming.Ray angle ->
                          Action.Snipe angle vector duration
                    _ ->
                      Debug.crash "Impossible"
              in
                Html.Events.onClick
                  (StartAiming 0 (Action.Snipe 0 position (1, 60)) update)
            ]
            [ Html.text "🎯 Snipe"
            ]
          ]
    ]
  ]
