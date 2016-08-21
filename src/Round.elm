

module Round exposing (..)


-- Elm
import Dict exposing (Dict)
import Array exposing (Array)
-- 1st
import Champ exposing (Champ)
import Vector
import Util
import Class exposing (Class)
import Warrior


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
moveChamp : String -> Dict String Champ -> Dict String Champ
moveChamp name dict =
  let
    champ = Util.forceUnwrap (Dict.get name dict)
  in
  if champ.status /= Champ.Moving then
    dict
  else
    case champ.waypoints of
      [] ->
        -- No more waypoints, so idle
        Dict.insert name { champ | status = Champ.Idling } dict
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
            action :: _ ->
              -- Waypoint had an action in queue, so transition into it
              let
                -- update waypoint actions
                waypoint' =
                  { waypoint
                      | actions = List.drop 1 waypoint.actions
                  }
              in
                Dict.insert
                  name
                  { champ
                      | position = waypoint.position
                      , status = Champ.ClassSpecific (Champ.Acting action)
                      , waypoints = waypoint' :: rest
                  }
                  dict
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


stepClass : String -> Class -> Dict String Champ -> Dict String Champ
stepClass name class dict =
  case class of
    Class.Warrior ->
      Warrior.stepChamp name dict
    _ ->
      -- unimplemented
      dict


-- TODO: This is getting really nasty.
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
      dict
      |> (stepClass name champ.class)
      |> (moveChamp name)
      |> (\d -> (log, d))


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
