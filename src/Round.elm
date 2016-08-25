

module Round exposing (..)


-- Elm
import Dict exposing (Dict)
import Array exposing (Array)
-- 1st
import Champ exposing (Champ)
import Vector
import Util
import Class exposing (Class)
import Class.Warrior
import Class.Ranger
import Action


type alias Tick =
  { id : Int
  , champs : Dict String Champ
  , log : List String
  }


type alias Round =
  { id : Int
  , ticks : Array Tick
  }


-- A champ only moves if status == Moving
moveChamp : String -> (List String, Dict String Champ) -> (List String, Dict String Champ)
moveChamp name (log, dict) =
  let
    champ = Util.forceUnwrap (Dict.get name dict)
  in
  if champ.status /= Champ.Moving then
    (log, dict)
  else
    case champ.waypoints of
      [] ->
        -- No more waypoints, so idle
        (log, Dict.insert name { champ | status = Champ.Idling } dict)
      waypoint :: rest ->
        -- if champ is on a waypoint, transition into any action that's on the
        -- way point.
        -- else, consume the waypoint and continue moving
        if (Vector.dist champ.position waypoint.position < champ.speed * 1/60) then
          case waypoint.actions of
            [] ->
              -- No actions, so consume the waypoint and head onwards
              Dict.insert
                name
                { champ
                    | position = waypoint.position
                    , waypoints = List.drop 1 champ.waypoints
                }
                dict
              |> \dict -> (log, dict)
            action :: _ ->
              -- Waypoint had an action in queue, so transition into it
              let
                _ = Debug.log "action consumed" action
                -- update waypoint actions
                waypoint' =
                  { waypoint
                      | actions = List.drop 1 waypoint.actions
                  }
                waypoints' =
                  -- If we consumed the waypoint's last action, then consume
                  -- the waypoint.
                  if List.isEmpty waypoint'.actions then
                    rest
                  else
                    waypoint' :: rest
              in
                Dict.insert
                  name
                  { champ
                      | position = waypoint.position
                      , status = Champ.ClassSpecific (Champ.Acting action)
                      , waypoints = waypoints'
                  }
                  dict
                |> \dict -> (log, dict)
        else
          -- didn't hit a waypoint, so keep moving towards the next one
          let
            (prevX, prevY) = champ.position
            (dx, dy) =
              Vector.fromPoints champ.position waypoint.position
              |> Vector.normalize
              |> Vector.scale (champ.speed * 1/60)
            position' =
              (prevX + dx, prevY + dy)
          in
            Dict.insert
              name
              (Champ.faceWaypoint { champ | position = position' })
              dict
            |> \dict -> (log, dict)


-- Checks champ for an action that is enqueued directly on them
-- If there is one, it transitions to it.
checkSelf : String -> (List String, Dict String Champ) -> (List String, Dict String Champ)
checkSelf name (log, dict) =
  let
    champ = Util.forceUnwrap (Dict.get name dict)
  in
    case champ.status of
      Champ.Dead ->
        (log, dict)
      Champ.ClassSpecific _ ->
        (log, dict)
      _ ->
        -- Can only start an action if idling/moving
        case champ.actions of
          [] ->
            -- No actions, nothing to do
            (log, dict)
          action :: rest ->
            let
              champ' =
                { champ
                    | status = Champ.ClassSpecific (Champ.Acting action)
                    , actions = rest
                }
            in
              (log, Dict.insert name champ' dict)


stepWaitAction : Action.Duration -> String -> Dict String Champ -> Dict String Champ
stepWaitAction (prevTicks, totalTicks) name dict =
  let
    champ =
      Util.forceUnwrap (Dict.get name dict)
    status' =
      if prevTicks == totalTicks then
        -- Finished waiting
        if List.isEmpty champ.waypoints then
          Champ.Idling
        else
          Champ.Moving
      else
        -- Still waiting
        Champ.ClassSpecific (Champ.Acting (Action.Wait (prevTicks + 1, totalTicks)))
    champ' =
      { champ
          | status = status'
      }
  in
    Dict.insert champ'.name champ' dict



-- Should really be stepAction, only delegating to class if
-- action is not general
stepClass : String -> Class -> (List String, Dict String Champ) -> (List String, Dict String Champ)
stepClass name class (log, dict) =
  let
    champ = Util.forceUnwrap (Dict.get name dict)
  in
  case champ.status of
    Champ.Dead ->
      -- the dead do nothing
      (log, dict)
    -- handle it here if it's a general action, else delegate to the class
    Champ.ClassSpecific (Champ.Acting (Action.Wait duration)) ->
      (log, stepWaitAction duration name dict)
    _ ->
      case class of
        Class.Warrior ->
          Class.Warrior.stepChamp name (log, dict)
        _ ->
          Class.Ranger.stepChamp name (log, dict)


stepChamp : String -> Champ -> (List String, Dict String Champ) -> (List String, Dict String Champ)
stepChamp name _ (log, dict) =
  let
    --  Load current champ from the dict since another champ's step
    --  may have mutated them
    champ = Util.forceUnwrap (Dict.get name dict)
  in
    if champ.status == Champ.Dead then
      -- Skip champ if they are dead
      (log, dict)
    else
      (log, dict)
      |> (checkSelf name)
      |> (stepClass name champ.class)
      |> (moveChamp name)


stepTick : Int -> List Tick -> List Tick
stepTick _  ticks =
  case ticks of
    [] ->
      Debug.crash "Impossible"
    prevTick :: _ ->
      let
        (log, champs) =
          (Dict.foldl stepChamp ([], prevTick.champs) prevTick.champs)
        nextTick =
          { id = prevTick.id + 1
          , champs = champs
          , log = log
          }
      in
        nextTick :: ticks


simulate : Int -> Dict String Champ -> Round
simulate ticksPerRound champs =
  let
    -- first we reset the round-scoped state of each champ
    resetChamps =
      Dict.map (\_ champ -> Champ.roundReset champ) champs
    initTick =
      Tick 0 resetChamps []
    ticks =
      List.foldl stepTick [initTick] [1..ticksPerRound]
      |> List.reverse
      -- Drop the final tick so we only have ticksPerRound quantity of ticks
      |> List.take ticksPerRound
  in
    { id = 1
    , ticks = Array.fromList ticks
    }
