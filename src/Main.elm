
module Main exposing (..)

-- Elm
import Html exposing (Html)
import Html.App
import Html.Attributes
import Svg exposing (..)
import Svg.Attributes
import Mouse
import Task
-- 3rd
import Keyboard.Extra as KE
-- 1st
import Grid exposing (Grid)
import Waypoint exposing (Waypoint)
import Champ exposing (Champ)


-- MODEL


type alias Drag =
  { start : Mouse.Position
  , current : Mouse.Position
  }


type alias Model =
  { rows : Int
  , cols : Int
  , grid : Grid
  , position : Mouse.Position
  , drag : Maybe Drag
  , scale : Float
  , champ : Champ
  , keyboard : KE.Model
  }




init : (Model, Cmd Msg)
init =
  let
    rows = 10
    cols = 15
    (kbModel, kbCmd) = KE.init
  in
  ( { grid = Grid.empty cols rows
    , rows = rows
    , cols = cols
    , position = Mouse.Position 0 0
    , drag = Nothing
    , scale = 1
    , champ =
        { position = (4, 4)
        , waypoints =
          [ { position = (7, 1) }
          , { position = (9, 6) }
          ]
        }
    , keyboard = kbModel
    }
  , Cmd.map Keyboard kbCmd
  )


-- UPDATE


type Msg
  = NoOp
  | TileClick Int Int
  | RemoveWaypoint Champ
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
      let
        _ = Debug.log "TileClick" (x, y)
        waypoint = { position = (x, y) }
        champ = model.champ
        champ' =
          { champ
              | waypoints = List.append champ.waypoints [waypoint]
          }
      in
        ( { model | champ = champ' }
        , Cmd.none
        )
    RemoveWaypoint champ ->
      let
        -- Drop the last waypoints
        waypoints' =
          List.take ((List.length champ.waypoints) - 1) champ.waypoints
        champ' =
          { champ | waypoints = waypoints'
          }
      in
        ( { model
              | champ = champ'
          }
        , Cmd.none
        )
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
        -- -- Spacebar toggles mode
        -- mode' =
        --   if KE.isPressed KE.Space kbModel' then
        --     case model.mode of
        --       Paint -> Move
        --       Move -> Paint
        --   else
        --     model.mode
        -- Super-Z rewinds from undo stack
        -- undoMsg =
        --   if KE.isPressed KE.Super kbModel' && KE.isPressed KE.CharZ kbModel' then
        --     Undo
        --   else
        --     NoOp
        -- Backspace removes the last waypoint for the selected champ,

        -- if it is ours
        backspaceMsg =
          if KE.isPressed KE.BackSpace kbModel' then
            RemoveWaypoint model.champ
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
        , champ = model.champ
        , onTileClick = TileClick
        }
    in
      Grid.view ctx model.grid
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
