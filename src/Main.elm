
module Main exposing (..)

-- Elm
import Html exposing (Html)
import Html.App
import Html.Attributes
import Html.Events
import Svg exposing (..)
import Svg.Attributes
import Mouse
import Task
import Dict exposing (Dict)
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
  , champs: Dict String Champ
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
      , waypoints =
        [ { position = (1, 2) }
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
    , champs = champs
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
  | SimulateRound
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
    AddWaypoint x y champ ->
      let
        waypoint =
          { position = (toFloat x, toFloat y)
          }
        champ' =
          { champ
              | waypoints = List.append champ.waypoints [waypoint]
          }
      in
        ( { model
              | champs = Dict.insert champ'.name champ' model.champs
              , selection = WaypointSelected champ' waypoint
          }
        , Cmd.none
        )
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
    RemoveWaypoint champ ->
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
          Dict.insert champ'.name champ' model.champs
      in
        ( { model
              | champs = champs'
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
    SimulateRound ->
      let
        _ = Debug.log "round" (Round.simulate model.champs)
      in
        (model, Cmd.none)
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
        , champs = model.champs
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
    , Html.button
      [ Html.Events.onClick SimulateRound ]
      [ Html.text "Simulate" ]
    , Html.pre
      []
      [ Html.text <| "Selection: " ++ toString model.selection
      ]
    ]
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
