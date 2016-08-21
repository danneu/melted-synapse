
module Main exposing (..)

-- Elm
import Html exposing (Html)
import Html.App
import Html.Attributes
import Html.Events
import Html.Events.Extra
import Mouse
import Task
import Dict exposing (Dict)
import Array exposing (Array)
import Json.Encode as JE
import Json.Decode as JD
import Time
-- 3rd
import Keyboard.Extra as KE
import List.Extra
-- 1st
import Vector exposing (Vector)
import Grid exposing (Grid)
import Waypoint exposing (Waypoint)
import Champ exposing (Champ)
import Round exposing (Round)
import Util.List
import Constants
import Action
import Util
import Sidebar
import WaypointDetail


-- MODEL


type alias Drag =
  { start : Mouse.Position
  , current : Mouse.Position
  }


type Selection
  = None
  | ChampSelected Champ
  | WaypointSelected Champ Waypoint


type Playback
  = Playing
  | Paused


type Mode
  = Planning (Dict String Champ)
  -- Int is current tick (0 to ticksPerRound-1)
  -- Tick 0 is the original state pre-simulation which can be used to
  -- transition back into planning mode.
  | Simulating Playback Int Round


type alias Model =
  { rows : Int
  , cols : Int
  , grid : Grid
  , position : Mouse.Position
  , drag : Maybe Drag
  , scale : Float
  , mode : Mode
  , selection : Selection
  , keyboard : KE.Model
  , showCoords : Bool
  , sidebar : Sidebar.Model
  }




init : (Model, Cmd Msg)
init =
  let
    rows = 13
    cols = 13
    (kbModel, kbCmd) = KE.init
    -- Seeding the game with some demo champs to play with
    champs =
      [ Champ.init "champ1" (6, 0) (75, 100)
        |> Champ.addWaypoint (6, 2) []
        |> Champ.addWaypoint (5, 5) []
      , Champ.init "champ2" (4, 2) (14, 100)
        |> Champ.addWaypoint (7, 4) []
      , Champ.init "champ3" (5, 8) (92, 100)
        |> Champ.addWaypoint (10, 9) []
      , Champ.init "champ4" (7, 8) (42, 100)
        |> Champ.addWaypoint (2, 9) []
      , Champ.init "champ5" (8, 9) (22, 100)
      , Champ.init "champ6" (10, 2) (100, 100)
        |> Champ.addWaypoint (10, 3) []
        |> Champ.addWaypoint (10, 5) []
      ]
      |> List.map (\ ({name} as champ) -> (name, champ))
      |> Dict.fromList
  in
  ( { grid = Grid.empty cols rows
    , rows = rows
    , cols = cols
    , position = Mouse.Position 50 50
    , drag = Nothing
    , scale = 0.80
    , mode = Planning champs
    , keyboard = kbModel
    , selection =
        ChampSelected (Util.forceUnwrap (List.head (Dict.values champs)))
    , showCoords = False
    , sidebar = Sidebar.init
    }
  , Cmd.map Keyboard kbCmd
  )


-- UPDATE


type Msg
  = NoOp
  | TileClick Int Int
  | ChampClick Champ
  | WaypointClick Champ Waypoint
  | AddWaypoint Int Int Champ
  | RemoveWaypoint Champ
  | ClearSelection
  -- SIMULATION
  | Pause
  | Unpause
  | ToggleMode
  | GoToTick Int
  -- DRAG
  | DragStart Mouse.Position
  | DragAt Mouse.Position
  | DragEnd Mouse.Position
  -- KEYBOARD
  | Keyboard KE.Msg
  -- ZOOM
  | Zoom Float
  -- DEBUG
  | ToggleCoords
  -- CHILDREN
  | SidebarMsg Sidebar.Msg



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)
    TileClick x y ->
      case model.selection of
        ChampSelected champ ->
          update (AddWaypoint x y champ) model
        WaypointSelected champ _ ->
          update (AddWaypoint x y champ) model
        _ ->
          (model, Cmd.none)
    ChampClick champ ->
      let
        (sidebar', _) =
          Sidebar.update (Sidebar.ChampSelected champ) model.sidebar
      in
        ( { model
              | selection = ChampSelected champ
              , sidebar = sidebar'
          }
        , Cmd.none
        )
    WaypointClick champ waypoint ->
      let
        (sidebar', _) =
          Sidebar.update
            (Sidebar.WaypointSelected champ.name waypoint)
            model.sidebar
      in
      ( { model
            | selection =
                WaypointSelected champ waypoint
            , sidebar =
                sidebar'
        }
      , Cmd.none
      )
    AddWaypoint x y champ ->
      -- Can only add waypoint in planning mode
      case model.mode of
        Simulating _ _ _ ->
          (model, Cmd.none)
        Planning champs ->
          let
            waypoint =
              Waypoint.empty (toFloat x, toFloat y)
            champ' =
              { champ
                  | waypoints = List.append champ.waypoints [waypoint]
              }
              |> Champ.faceWaypoint
            champs' =
              Dict.insert champ'.name champ' champs
          in
            ( { model
                  | mode = Planning champs'
                  , selection = WaypointSelected champ' waypoint
              }
            , Cmd.none
            )
    RemoveWaypoint champ ->
      -- Can only modify waypoints in planning mode
      case model.mode of
        Simulating _ _ _ ->
          (model, Cmd.none)
        Planning champs ->
          let
            -- Drop the last waypoint
            waypoints' =
              Util.List.dropRight 1 champ.waypoints
            champ' =
              { champ | waypoints = waypoints'
              }
              |> Champ.faceWaypoint
            (selection', sidebarMsg) =
              case List.Extra.last waypoints' of
                Nothing ->
                  ( ChampSelected champ'
                  , Sidebar.ChampSelected champ'
                  )
                Just waypoint ->
                  ( WaypointSelected champ' waypoint
                  , Sidebar.WaypointSelected champ'.name waypoint
                  )
            (sidebar', _) =
              Sidebar.update sidebarMsg model.sidebar
            champs' =
              Dict.insert champ'.name champ' champs
          in
            ( { model
                  | mode = Planning champs'
                  , selection = selection'
                  , sidebar = sidebar'
              }
            , Cmd.none
            )
    ClearSelection ->
      let
        (sidebar', _) =
          Sidebar.update Sidebar.Clear model.sidebar
      in
        ( { model
              | selection = None
              , sidebar = sidebar'
          }
        , Cmd.none
        )
    Pause ->
      let
        mode' =
          case model.mode of
            Simulating Playing idx round ->
              Simulating Paused idx round
            _ ->
              model.mode
      in
        ( { model
              | mode = mode'
          }
        , Cmd.none
        )
    Unpause ->
      let
        mode' =
          case model.mode of
            Simulating Paused idx round ->
              -- Unpause replays from tick 0 if we're at the end
              if idx == Constants.ticksPerRound - 1 then
                Simulating Playing 0 round
              else
                Simulating Playing idx round
            _ ->
              model.mode
      in
        ( { model
              | mode = mode'
          }
        , Cmd.none
        )
    ToggleMode ->
      let
        mode' =
          case model.mode of
            Planning champs ->
              --Simulating Paused 0 (Round.simulate champs)
              Simulating Playing 0 (Round.simulate Constants.ticksPerRound champs)
            Simulating _ _ round ->
              case Array.get 0 round.ticks of
                Nothing ->
                  Debug.crash "Impossible"
                Just tick ->
                  Planning tick.champs
      in
        ( { model | mode = mode' }
        , Cmd.none
        )
    GoToTick idx ->
      -- Only works in simulationg mode
      case model.mode of
        Planning _ ->
          (model, Cmd.none)
        Simulating playback _ round ->
          let
            maxIdx =
              (Array.length round.ticks) - 1
            (playback', idx') =
              if idx > maxIdx then
                (Paused, maxIdx)
              else
                (playback, idx)
            model' =
              { model
                  | mode = Simulating playback' idx' round
              }
          in
            (model', Cmd.none)
    -- DRAG
    DragStart xy ->
      { model
          | drag = Just (Drag xy xy)
      } ! [Cmd.none]
    DragAt xy ->
      { model
          | drag = Maybe.map (\ {start} -> Drag start xy) model.drag
      } ! [Cmd.none]
    DragEnd _ ->
      { model
          | drag = Nothing
          , position =
              -- Only update position if we're in Move mode
              -- case model.mode of
              --   Move -> getPosition model
              --   _ -> model.position
              getPosition model
      } ! [Cmd.none]
    -- KEYBOARD
    -- TODO: Figure out intended usage of this library instead of stuffing
    -- everything in this action
    Keyboard kbMsg ->
      let
        (kbModel', kbCmd') = KE.update kbMsg model.keyboard
        -- spacebar toggles between modes
        spaceMsg =
          if KE.isPressed KE.Space kbModel' then
            ToggleMode
          else
            NoOp
        -- Backspace removes the last waypoint for the selected champ,
        -- if it is ours
        backspaceMsg =
          if KE.isPressed KE.BackSpace kbModel' then
            case model.selection of
              ChampSelected champ ->
                if List.isEmpty champ.waypoints then
                  ClearSelection
                else
                  RemoveWaypoint champ
              WaypointSelected champ _ ->
                RemoveWaypoint champ
              _ ->
                NoOp
          else
            NoOp
      in
        ( { model
              | keyboard = kbModel'
          }
        , Cmd.batch
            [ Cmd.map Keyboard kbCmd'
            , Task.perform identity identity (Task.succeed backspaceMsg)
            , Task.perform identity identity (Task.succeed spaceMsg)
            ]
        )
    -- ZOOM
    Zoom amount ->
      ( { model
            | scale = Basics.max 0 (model.scale + amount)
        }
      , Cmd.none
      )
    -- DEBUG
    ToggleCoords ->
      { model
          | showCoords = not model.showCoords
      } ! [Cmd.none]
    --
    -- CHILDREN
    --
    SidebarMsg sidebarMsg ->
      let
        (sidebar', outMsg) =
          Sidebar.update sidebarMsg model.sidebar
        -- When the detail updates, the game select should, too
        selection' =
          -- If user is clearing the sidebar, then it doesn't make sense
          -- to keep the selection
          case sidebarMsg of
            Sidebar.Clear ->
              None
            _ ->
              case model.mode of
                Planning champs ->
                  case sidebar'.detail of
                    Sidebar.ChampDetail champ ->
                      ChampSelected champ
                    Sidebar.WaypointDetail {champName, waypoint} ->
                      WaypointSelected
                        (Util.forceUnwrap (Dict.get champName champs))
                        waypoint
                    _ ->
                      model.selection
                _ ->
                  model.selection
        model' =
          case outMsg of
            Sidebar.WaypointDetailOutMsg (WaypointDetail.UpdateWaypoint champName waypoint) ->
              updateWaypoint champName waypoint.position waypoint model
            _ ->
              model
      in
        ( { model'
              | sidebar = sidebar'
              , selection = selection'
          }
        , Cmd.none
        )


-- No-ops unless game is in planning mode
-- Only allows for action updates for now
updateWaypoint : String -> Vector -> Waypoint -> Model -> Model
updateWaypoint champName position newWaypoint model =
  case model.mode of
    Planning champs ->
      let
        champ =
          Util.forceUnwrap (Dict.get champName champs)
        updater = \oldWaypoint ->
          if oldWaypoint.position == position then
            { oldWaypoint | actions = newWaypoint.actions }
          else
            oldWaypoint
        champ' =
          { champ | waypoints = List.map updater champ.waypoints }
        champs' =
          Dict.insert champName champ' champs
      in
        { model | mode = Planning champs' }
    _ ->
      model





getPosition : Model -> Mouse.Position
getPosition {position, drag} =
  case drag of
    Nothing ->
      position
    Just {start, current} ->
      Mouse.Position
        (position.x + current.x - start.x)
        (position.y + current.y - start.y)



-- VIEW


view : Model -> Html Msg
view model =
  Html.div
  []
  [ let
      ctx =
        { scale = model.scale
        , rows = model.rows
        , cols = model.cols
        , offset =
            -- Only offset by drag position if we're in Move mode
            -- case model.mode of
            --   Move ->
            --     getPosition model
            --   _ ->
            --     model.position
            getPosition model
        , champs =
            case model.mode of
              Planning champs ->
                champs
              Simulating _ idx round ->
                case Array.get idx round.ticks of
                  Nothing ->
                    Debug.crash "Impossible"
                  Just tick ->
                    tick.champs
        , tickIdx =
            case model.mode of
              Simulating _ idx _ ->
                idx
              _ ->
                0
        , selectedChamp =
            case model.selection of
              ChampSelected champ ->
                Just champ
              WaypointSelected champ _ ->
                Just champ
              _ ->
                Nothing
        , selectedWaypoint =
            case model.selection of
              WaypointSelected _ waypoint ->
                Just waypoint
              _ ->
                Nothing
        , onTileClick = TileClick
        , onChampClick = ChampClick
        , onWaypointClick = WaypointClick
        , onMouseDown =
            Html.Events.on "mousedown" (JD.map DragStart Mouse.position)
        , showCoords =
            model.showCoords
        }
    in
      Grid.view ctx model.grid
  , Html.App.map SidebarMsg (Sidebar.view model.sidebar)
  , Html.div
    [ Html.Attributes.id "footerbar" ]
    [ Html.button
      [ Html.Events.onClick (Zoom 0.1) ]
      [ Html.span
        [ Html.Attributes.class "glyphicon glyphicon-zoom-in" ]
        []
      ]
    , Html.button
      [ Html.Events.onClick (Zoom -0.1) ]
      [ Html.span
        [ Html.Attributes.class "glyphicon glyphicon-zoom-out" ]
        []
      ]
    , Html.button
      [ Html.Events.onClick ToggleMode
      , Html.Attributes.class "btn-primary"
      ]
      [ Html.text
          <| case model.mode of
            Planning _ -> "Planning Mode"
            Simulating _ _ _ -> "Simulation Mode"
      ]
    , Html.div
      [ Html.Attributes.style
          [ ("display", "inline-block")
          , ("margin-left", "10px")
          ]
      ]
      [ Html.text "Source: "
      , Html.a
        [ Html.Attributes.href "https://github.com/danneu/melted-synapse"
        , Html.Attributes.target "_blank"
        ]
        [ Html.text "danneu/melted-synapse" ]
      ]
    , Html.span
      []
      [ Html.text " Show coords: "
      , Html.input
        [ Html.Attributes.type' "checkbox"
        , Html.Events.onCheck (\_ -> ToggleCoords)
        , Html.Attributes.checked model.showCoords
        ]
        []
      ]
    , viewTickScrubber model
    -- Only show instructions if in planning mode
    , case model.mode of
        Planning _ ->
          Html.ul
          []
          [ Html.li
            []
            [ Html.text "Click a champ or waypoint to select it." ]
          , Html.li
            []
            [ Html.text "Double-click an empty tile to add a waypoint for selected champ." ]
          , Html.li
            []
            [ Html.text "Press escape to remove last waypoint for selected champ." ]
          , Html.li
            []
            [ Html.text "Press spacebar to toggle from planning-mode into simulation-mode." ]
          , Html.li
            []
            [ Html.text "Click and drag anywhere on the map to pan." ]
          , Html.li
            []
            [ Html.text "You must be in planning-mode to modify waypoints." ]
          ]
        _ ->
          Html.text ""
    ]
  ]


viewTickScrubber : Model -> Html Msg
viewTickScrubber model =
  -- only visible in simulation mode
  case model.mode of
    Planning _ ->
      Html.text ""
    Simulating playback idx round ->
      let
        tick =
          case Array.get idx round.ticks of
            Nothing ->
              Debug.crash "Impossible"
            Just tick ->
              tick
      in
      Html.div
        [ Html.Attributes.style [ ("margin", "10px 50px 10px 10px") ]
        ]
        [ Html.input
          [ Html.Attributes.type' "range"
          , Html.Attributes.min "0"
          , Html.Attributes.max (toString (Constants.ticksPerRound - 1))
          , Html.Attributes.value (toString idx)
          , Html.Attributes.list "tick-scrubber"
          , Html.Attributes.property "step" (JE.string "1")
          , Html.Events.on
              "input"
              (JD.map GoToTick Html.Events.Extra.targetValueInt)
          ]
          []
        , Html.datalist
          [ Html.Attributes.id "tick-scrubber"
          ]
          ( List.map
              (\n ->
                (Html.option [] [ Html.text (toString n) ])
              )
              [0..Constants.ticksPerRound-1]
          )
        , if playback == Playing then
            Html.button
            [ Html.Events.onClick Pause
            ]
            [ Html.text "Pause" ]
          else
            Html.button
            [ Html.Events.onClick Unpause
            ]
            [ Html.text
                (if idx == Constants.ticksPerRound - 1 then "Replay" else "Resume")
            ]
        , Html.div
          [ Html.Attributes.style [ ("display", "inline-block")
                                  , ("margin-left", "10px")
                                  ]
          ]
          [ Html.text ( "Round " ++ toString round.id ++
                        ", Tick " ++ toString tick.id
                      )
          ]
        , Html.p
          []
          [ Html.text "Drag the slider to scrub through history" ]
          -- Display a log of events in the round as they happen
        , let
            currLog : List (Int, String)
            currLog =
              Array.toList round.ticks
              -- Only show the log messages that have happened before curr tick
              --|> List.take (idx + 1)
              |> List.concatMap
                   (\ {id, log} ->
                      List.map (\msg -> (id, msg)) log
                   )
            viewLogItem =
              \ (id, msg) ->
                Html.li
                [ Html.Attributes.style
                    [ ("opacity", if id <= tick.id then "1.0" else "0.5")
                    ]
                ]
                [ Html.button
                  [ Html.Events.onClick (GoToTick id) ]
                  [ Html.text ("Tick " ++ toString id ) ]
                , Html.text (" " ++ msg)
                ]
          in
            Html.ul
            []
            (List.map viewLogItem currLog)
        ]


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Sub.map Keyboard KE.subscriptions
    , case model.drag of
        Nothing ->
          Sub.none
        Just _ ->
          Sub.batch
            [ Mouse.moves DragAt
            , Mouse.ups DragEnd
            ]
    , case model.mode of
        Simulating Playing idx _ ->
          Time.every (Time.second / 60) (\_ -> (GoToTick (idx + 1)))
        _ ->
          Sub.none
    ]


main : Program Never
main =
  Html.App.program
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }
