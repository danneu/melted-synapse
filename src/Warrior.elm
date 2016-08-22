

module Warrior exposing  (..)


-- Elm
import Dict exposing (Dict)
-- 1st
import Champ exposing (Champ)
import Vector exposing (Vector)
import Action exposing (Action)
import Util


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


checkAutoAttack : Champ -> Dict String Champ -> Dict String Champ
checkAutoAttack champ dict =
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
        dict
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
        in
          dict
          |> Dict.insert champ'.name champ'
          |> Dict.insert victim'.name victim'



stepChamp : String -> Dict String Champ -> Dict String Champ
stepChamp name dict =
  let
    champ =
      Util.forceUnwrap (Dict.get name dict)
  in
    case champ.status of
      Champ.Dead ->
        dict
      Champ.ClassSpecific classStatus ->
        case classStatus of
          Champ.AutoAttacking duration victim ->
            tickAutoAttack champ duration victim dict
          Champ.Acting action ->
            case action of
              Action.Charge angle ->
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
                  champ' =
                    { champ
                        | position = position'
                        , angle = angle
                    }
                in
                  Dict.insert name champ' dict
              _ ->
                Debug.crash "Unexpected classStatus"
      _ ->
        -- Idling | Moving, so check if we can autoattack an enemy
        checkAutoAttack champ dict
