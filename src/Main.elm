
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
-- 3rd
import Keyboard.Extra as KE
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


type Mode
  = Planning (Dict String Champ)
  -- Int is current tick (0 to 179)
  -- Tick 0 is the original state pre-simulation which can be used to
  -- transition back into planning mode.
  | Simulating Int (Array (Dict String Champ))


type alias Model =
  { rows : Int
  , cols : Int
  , grid : Grid
  , position : Mouse.Position
  , drag : Maybe Drag
  , scale : Float
  , mode : Mode
  , selection : Selection
  --, hoveredTile : (Int, Int) -- x,y
  , keyboard : KE.Model
  }




init : (Model, Cmd Msg)
init =
  let
    rows = 10
    cols = 15
    (kbModel, kbCmd) = KE.init
    champ1 =
      { name = "champ1"
      , hp = (100, 100)
      , position = (4.5, 4.5)
      , speed = 2
      , angle = 0
      , waypoints =
        [ { position = (7, 1) }
        , { position = (9, 6) }
        ]
      }
    champ2 =
      { name = "champ2"
      , hp = (100, 100)
      , position = (2, 7)
      , speed = 2
      , angle = 0
      , waypoints =
        [ { position = (3, 6) }
        , { position = (4, 7) }
        , { position = (3, 8) }
        ]
      }
    champs =
      Dict.fromList
        [ (champ1.name, champ1)
        , (champ2.name, champ2)
        ]
  in
  ( { grid = Grid.empty cols rows
    , rows = rows
    , cols = cols
    , position = Mouse.Position 0 0
    , drag = Nothing
    , scale = 1
    , mode = Planning champs
    , keyboard = kbModel
    , selection = ChampSelected champ1
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
        Simulating _ _ ->
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
        Simulating _ _ ->
          (model, Cmd.none)
        Planning champs ->
          let
            -- Drop the last waypoint
            waypoints' =
              Util.List.dropRight 1 champ.waypoints
            champ' =
              { champ | waypoints = waypoints'
              }
            selection' =
              case List.head waypoints' of
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
              Simulating 0 (Round.simulate champs)
            Simulating _ ticks ->
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
        Simulating _ ticks ->
          let
            model' =
              { model
                  | mode = Simulating idx ticks
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
        -- Backspace removes the last waypoint for the selected champ,
        -- if it is ours
        backspaceMsg =
          if KE.isPressed KE.BackSpace kbModel' then
            case model.selection of
              ChampSelected champ ->
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
            ]
        )



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
              Simulating idx ticks ->
                case Array.get idx ticks of
                  Nothing ->
                    Debug.crash "Impossible"
                  Just champs ->
                    champs
        , onTileClick = TileClick
        , onChampClick = ChampClick
        , onWaypointClick = WaypointClick
        }
    in
      Grid.view ctx model.grid
  , Html.div
    [ Html.Attributes.id "sidebar" ]
    [ Html.button
      [ Html.Events.onClick ClearSelection ]
      [ Html.text "Clear Selection" ]
    -- , Html.button
    --   [ Html.Events.onClick SimulateRound ]
    --   [ Html.text "Simulate" ]
    , Html.button
      [ Html.Events.onClick ToggleMode ]
      [ Html.text
          <| case model.mode of
            Planning _ -> "Simulation Mode"
            Simulating _ _ -> "Planning Mode"
      ]
    , Html.pre
      []
      [ Html.text <| "Selection: " ++ toString model.selection
      ]
    , viewTickScrubber model
    ]
  ]


viewTickScrubber : Model -> Html Msg
viewTickScrubber model =
  -- only visible in simulation mode
  case model.mode of
    Planning _ ->
      Html.text ""
    Simulating idx _ ->
      Html.div
        []
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
    ]


main : Program Never
main =
  Html.App.program
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }