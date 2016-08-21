
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
import Set
-- 3rd
import Keyboard.Extra as KE
import List.Extra
-- 1st
import Grid exposing (Grid)
import Waypoint exposing (Waypoint)
import Champ exposing (Champ)
import Round
import Util.List


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
  -- Int is current tick (0 to 179)
  -- Tick 0 is the original state pre-simulation which can be used to
  -- transition back into planning mode.
  | Simulating Playback Int (Array (Dict String Champ))


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
  }




init : (Model, Cmd Msg)
init =
  let
    rows = 13
    cols = 13
    (kbModel, kbCmd) = KE.init
    champ1 =
      { name = "champ1"
      , action = Champ.Moving
      , hp = (75, 100)
      , position = (6, 0)
      , speed = 2
      , angle = 0
      , waypoints =
        [ { position = (6, 2) }
        , { position = (5, 5) }
        ]
      , autoattacked = Set.empty
      }
      |> Champ.faceWaypoint
    champ2 =
      { name = "champ2"
      , action = Champ.Moving
      , hp = (14, 100)
      , position = (4, 2)
      , speed = 2
      , angle = 0
      , waypoints =
        [ { position = (7, 4) }
        ]
      , autoattacked = Set.empty
      }
      |> Champ.faceWaypoint
    -- champ3 and champ4 will run directly at each other
    -- to demonstrate head-on champ collision (auto-attack each other)
    champ3 =
      { name = "champ3"
      , action = Champ.Moving
      , hp = (50, 100)
      , position = (4, 8)
      , speed = 2
      , angle = 0
      , waypoints =
        [ { position = (10, 9) }
        ]
      , autoattacked = Set.empty
      }
      |> Champ.faceWaypoint
    champ4 =
      { name = "champ4"
      , action = Champ.Moving
      , hp = (92, 100)
      , position = (8, 8)
      , speed = 2
      , angle = 0
      , waypoints =
        [ { position = (2, 9) }
        ]
      , autoattacked = Set.empty
      }
      |> Champ.faceWaypoint
    champs =
      Dict.fromList
        [ (champ1.name, champ1)
        , (champ2.name, champ2)
        , (champ3.name, champ3)
        , (champ4.name, champ4)
        ]
  in
  ( { grid = Grid.empty cols rows
    , rows = rows
    , cols = cols
    , position = Mouse.Position 50 50
    , drag = Nothing
    , scale = 0.80
    , mode = Planning champs
    , keyboard = kbModel
    , selection = ChampSelected champ1
    , showCoords = False
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
  --| SimulateRound
  | ToggleMode
  | GoToTick Int
  -- DRAG
  | DragStart Mouse.Position
  | DragAt Mouse.Position
  | DragEnd Mouse.Position
  -- KEYBOARD
  | Keyboard KE.Msg
  -- ZOOM
  | ZoomIn
  | ZoomOut
  -- DEBUG
  | ToggleCoords



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
      ( { model
            | selection = ChampSelected champ
        }
      , Cmd.none
      )
    WaypointClick champ waypoint ->
      ( { model
            | selection = WaypointSelected champ waypoint
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
              { position = (toFloat x, toFloat y)
              }
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
            selection' =
              case List.Extra.last waypoints' of
                Nothing ->
                  ChampSelected champ'
                Just waypoint ->
                  WaypointSelected champ' waypoint
            champs' =
              Dict.insert champ'.name champ' champs
          in
            ( { model
                  | mode = Planning champs'
                  , selection = selection'
              }
            , Cmd.none
            )
    ClearSelection ->
      ( { model
            | selection = None
        }
      , Cmd.none
      )
    ToggleMode ->
      let
        mode' =
          case model.mode of
            Planning champs ->
              --Simulating Paused 0 (Round.simulate champs)
              Simulating Playing 0 (Round.simulate champs)
            Simulating _ _ ticks ->
              case Array.get 0 ticks of
                Nothing ->
                  Debug.crash "Impossible"
                Just champs ->
                  Planning champs
      in
        ( { model | mode = mode' }
        , Cmd.none
        )
    GoToTick idx ->
      -- Only works in simulationg mode
      case model.mode of
        Planning _ ->
          (model, Cmd.none)
        Simulating playback _ ticks ->
          let
            maxIdx =
              (Array.length ticks) - 1
            (playback', idx') =
              if idx > maxIdx then
                (Paused, maxIdx)
              else
                (playback, idx)
            model' =
              { model
                  | mode = Simulating playback' idx' ticks
              }
          in
            (model', Cmd.none)
    -- SimulateRound ->
    --   let
    --     _ = Debug.log "round" (Round.simulate model.champs)
    --   in
    --     (model, Cmd.none)
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
    ZoomIn ->
      { model
          | scale = Basics.max 0 (model.scale + 0.1)
      } ! [Cmd.none]
    ZoomOut ->
      { model
          | scale = Basics.max 0 (model.scale - 0.1)
      } ! [Cmd.none]
    -- DEBUG
    ToggleCoords ->
      { model
          | showCoords = not model.showCoords
      } ! [Cmd.none]




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
              Simulating _ idx ticks ->
                case Array.get idx ticks of
                  Nothing ->
                    Debug.crash "Impossible"
                  Just champs ->
                    champs
        , ticksPerRound =
            180
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
  , Html.div
    [ Html.Attributes.id "sidebar" ]
    [ Html.button
      [ Html.Events.onClick ZoomIn ]
      [ Html.text "Zoom In" ]
    , Html.button
      [ Html.Events.onClick ZoomOut ]
      [ Html.text "Zoom Out" ]
    , Html.button
      [ Html.Events.onClick ClearSelection ]
      [ Html.text "Clear Selection" ]
    -- , Html.button
    --   [ Html.Events.onClick SimulateRound ]
    --   [ Html.text "Simulate" ]
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
    Simulating _ idx _ ->
      Html.div
        [ Html.Attributes.style [ ("margin", "10px 50px 10px 10px") ]
        ]
        [ Html.input
          [ Html.Attributes.type' "range"
          , Html.Attributes.min "0"
          , Html.Attributes.max "180"
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
              [0..179]
          )
        , Html.p
          []
          [ Html.text "Drag the slider to scrub through history" ]
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
