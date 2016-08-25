

module Class.Warrior exposing  (..)


-- Elm
import Dict exposing (Dict)
-- 1st
import Champ exposing (Champ)
import Vector exposing (Vector)
import Action exposing (Action)
import Util
import Collision


-- Progresses the cooldown and transitions to another status once finished.
tickAutoAttack :
  Champ -> (Int, Int) -> Champ -> Dict String Champ -> Dict String Champ
tickAutoAttack champ (prevTick, tickDuration) victim dict =
  let
    currTick =
      prevTick + 1
    status' =
      if currTick <= tickDuration then
        -- We are still auto-attacking
        Champ.ClassSpecific (Champ.AutoAttacking (currTick, tickDuration) victim)
      else
        -- Done auto-attacking, so transition to Idling or Moving
        if List.isEmpty champ.waypoints then
          Champ.Idling
        else
          Champ.Moving
    champ' =
      { champ | status = status' }
  in
    Dict.insert champ'.name champ' dict


checkAutoAttack : Champ -> (List String, Dict String Champ) -> (List String, Dict String Champ)
checkAutoAttack champ (log, dict) =
  let
    attackRange =
      1 -- meters aka tiles
    champsInRange =
      List.filter
        ( \other ->
            -- ignore self
            champ.name /= other.name
            -- ignore dead champs
            && other.status /= Champ.Dead
            -- ignore champs out of range
            && (Vector.dist champ.position other.position) <= attackRange
        )
        (Dict.values dict)
  in
    case champsInRange of
      [] ->
        (log, dict)
      victim :: _ ->
        let
          -- damage the victim
          victim' =
            Champ.sufferDamage 25 victim
          champ' =
            { champ
                | status =
                    Champ.ClassSpecific (Champ.AutoAttacking (1, 60) victim')
                  -- Face the victim we're attacking
                , angle =
                    Vector.angleTo champ.position victim'.position
            }
          log' =
            if victim'.status == Champ.Dead then
              (champ.name ++ " killed " ++ victim'.name ++ " with AutoAttack 👊")
              :: log
            else
              log
        in
          dict
          |> Dict.insert champ'.name champ'
          |> Dict.insert victim'.name victim'
          |> \dict -> (log', dict)


stepCharge : Float -> Champ -> (List String, Dict String Champ) -> (List String, Dict String Champ)
stepCharge angle champ (log, dict) =
  let
    deltaTime =
      1/60
    chargeSpeed =
      5
    velocity =
      ( chargeSpeed * cos angle * deltaTime
      , chargeSpeed * sin angle * deltaTime
      )
    position' =
      Vector.add champ.position velocity
    enemies =
      dict
      -- Cannot collide with self
      |> Dict.remove champ.name
      -- Cannot collide with the dead
      |> Dict.filter (\_ enemy -> enemy.status /= Champ.Dead)
      |> Dict.values
  in
    case Collision.test enemies position' of
      Just (Collision.Enemy victim) ->
        let
          victim' =
            Champ.sufferDamage 100 victim
          champ' =
            { champ
                | position = position'
                , status =
                    -- Maybe abilities should always transition champ to
                    -- Idling when they are done, and Round's moveChamp
                    -- step can transition from Idling -> Moving if they have
                    -- waypoints so each ability doesn't need to implement this.
                    if List.isEmpty champ.waypoints then
                      Champ.Idling
                    else
                      Champ.Moving
            }
          log' =
            if victim'.status == Champ.Dead then
              (champ.name ++ " killed " ++ victim'.name ++ " with Charge 🚀")
              :: log
            else
              log
        in
          dict
          -- Update the victim
          |> Dict.insert victim'.name victim'
          -- Stop moving the charging champ
          |> Dict.insert champ'.name champ'
          |> \dict -> (log', dict)
      _ ->
        -- Keep moving the champ
        let
          champ' =
            { champ
                | position = position'
                , angle = angle
            }
        in
          (log, Dict.insert champ'.name champ' dict)



stepChamp : String -> (List String, Dict String Champ) -> (List String, Dict String Champ)
stepChamp name (log, dict) =
  let
    champ =
      Util.forceUnwrap (Dict.get name dict)
  in
    case champ.status of
      Champ.Dead ->
        (log, dict)
      Champ.ClassSpecific classStatus ->
        case classStatus of
          Champ.AutoAttacking duration victim ->
            (log, tickAutoAttack champ duration victim dict)
          Champ.Acting action ->
            case action of
              Action.Charge angle ->
                stepCharge angle champ (log, dict)
              _ ->
                Debug.crash "Unexpected classStatus"
      _ ->
        -- Idling | Moving, so check if we can autoattack an enemy
        checkAutoAttack champ (log, dict)
