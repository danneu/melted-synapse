

module Round exposing (..)


-- Elm
import Dict exposing (Dict)
import Array exposing (Array)
import Set
-- 1st
import Champ exposing (Champ)
import Vector


type alias Tick =
  { id : Int
  , champs : Dict String Champ
  , log : List String
  }


type alias Round =
  { id : Int
  , ticks : Array Tick
  }


-- TODO: This is getting really nasty.
stepChamp : String -> Champ -> (List String, Dict String Champ) -> (List String, Dict String Champ)
stepChamp name _ (log, dict) =
  let
    -- 0. Load current champ from the dict since another champ's step
    --    may have mutated them
    champ0 =
      case Dict.get name dict of
        Nothing ->
          Debug.crash "Impossible"
        Just champ ->
          champ
  in
    -- Skip champ is they are dead
    if champ0.action == Champ.Dead then
      (log, dict)
    else
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
      (champ2, maybeVictim) =
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
              ( { champ1 | action = action' }
                -- Point champ at victim every frame
                |> Champ.faceVictim
              , Nothing
              )
          -- Champ is available to auto-attack something, so check enemies in range
          _ ->
            -- if another champ is within autoattack range, attack that champ
            let
              autoattackRange =
                1 -- radius in meters
              champsToAttack =
                List.filter
                  ( \other ->
                      -- ignore self
                      champ1.name /= other.name
                      -- ignore dead champs
                      && other.action /= Champ.Dead
                      -- ignore champs out of range
                      && (Vector.dist champ1.position other.position) <= autoattackRange
                  )
                  (Dict.values dict)
            in
              case List.head champsToAttack of
                Nothing ->
                  (champ1, Nothing)
                Just enemy ->
                  ( { champ1
                      | action =
                          Champ.AutoAttacking (1, 60) enemy
                    }
                    |> Champ.faceVictim
                  , Just enemy
                  )
      -- FIXME: the code/exprs assigned to maybeVictim' and log' are examples
      --        of Elm code that feels wrong to me but I can't seem to avoid.
      maybeVictim' =
        Maybe.map (Champ.sufferDamage 25) maybeVictim
      log' =
        case maybeVictim' of
          Just {name, action} ->
            case action of
              Champ.Dead ->
                List.append log [champ0.name ++ " killed " ++ name]
              _ ->
                log
          _ ->
            log
    in
      ( log'
      , dict
        -- Update dict with this champ's move
        |> Dict.insert champ0.name champ2
        -- Mutate attack victim if there was one
        |> case maybeVictim' of
            Nothing ->
              identity
            Just victim ->
              Dict.insert victim.name victim
      )


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
