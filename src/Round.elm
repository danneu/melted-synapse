

module Round exposing (..)


-- Elm
import Dict exposing (Dict)
import Array exposing (Array)
import Set
-- 1st
import Champ exposing (Champ)
import Vector


stepChamp : String -> Champ -> Dict String Champ -> Dict String Champ
stepChamp name champ0 dict =
  let
    -- 1. Move the champ (champ0 -> champ1)
    champ1 =
      case champ0.action of
        Champ.Moving ->
          case champ0.waypoints of
            -- idle when we run out of waypoints
            [] ->
              { champ0 | action = Champ.Idling }
            waypoint :: rest ->
              -- consume waypoint if champ is on it, else move champ towards it
                if Vector.dist champ0.position waypoint.position < champ0.speed * 1/60 then
                  { champ0
                      | position = waypoint.position
                      , waypoints = List.drop 1 champ0.waypoints
                  }
                else
                  let
                    (prevX, prevY) = champ0.position
                    (dx, dy) =
                      Vector.fromPoints champ0.position waypoint.position
                      |> Vector.normalize
                      |> Vector.scale (champ0.speed * 1/60)
                    position' =
                      (prevX + dx, prevY + dy)
                  in
                    { champ0 | position = position' }
                    |> Champ.faceWaypoint
        _ ->
          champ0
    -- 2. Check and advance auto-attack  (champ1 -> champ2)
    champ2 =
      case champ1.action of
        -- Champ is in the middle of an auto-attack, so advance it.
        -- If it's finished, then transition to another state.
        Champ.AutoAttacking (currTick, tickDuration) victim ->
          let
            currTick' =
              currTick + 1
            action' =
              if currTick' <= tickDuration then
                -- still autoattacking
                let
                  victim' =
                    case Dict.get victim.name dict of
                      Just enemy ->
                        enemy
                      _ ->
                        Debug.crash "Impossible"
                in
                  Champ.AutoAttacking (currTick', tickDuration) victim'
              else
                -- done autoattacking, so transition to idling or moving
                if List.isEmpty champ1.waypoints then
                  Champ.Idling
                else
                  Champ.Moving
          in
            { champ1 | action = action' }
            -- Point champ at victim every frame
            |> Champ.faceVictim
        -- Champ is available to auto-attack something, so check enemies in range
        _ ->
            -- if another champ is within autoattack range, attack that champ
            -- TODO: but only once per round
          let
            autoattackRange =
              1 -- radius in meters
            champsToAttack =
              List.filter
                ( \other ->
                    -- ignore self
                    champ1.name /= other.name
                    -- ignore champs we've already autoattacked
                    && not (Set.member other.name champ1.autoattacked)
                    -- ignore champs out of range
                    && (Vector.dist champ1.position other.position) <= autoattackRange
                )
                (Dict.values dict)
          in
            case List.head champsToAttack of
              Nothing ->
                champ1
              Just enemy ->
                { champ1
                    | autoattacked =
                        Set.insert enemy.name champ1.autoattacked
                    , action =
                        Champ.AutoAttacking (1, 60) enemy
                }
                |> Champ.faceVictim
  in
    Dict.insert champ0.name champ2 dict


stepTick : Int -> List (Dict String Champ) -> List (Dict String Champ)
stepTick _  ticks =
  case ticks of
    [] ->
      Debug.crash "Impossible"
    prevChamps :: _ ->
      (Dict.foldl stepChamp prevChamps prevChamps) :: ticks


simulate : Int -> Dict String Champ -> Array (Dict String Champ)
simulate ticksPerRound champs =
  let
    -- first we reset the round-scoped state of each champ
    resetChamps =
      Dict.map (\_ champ -> Champ.roundReset champ) champs
  in
    List.foldl stepTick [resetChamps] [1..ticksPerRound]
    |> List.reverse
    -- Drop the final tick so we only have ticksPerRound quantity of ticks
    |> List.take ticksPerRound
    |> Array.fromList
