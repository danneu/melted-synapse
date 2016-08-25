

module Class.Ranger exposing  (..)


-- Elm
import Dict exposing (Dict)
-- 1st
import Champ exposing (Champ)
import Vector exposing (Vector)
import Action exposing (Action)
import Util
import Collision


-- Calculate the arrow's new position, check and handle arrow
-- collision with enemy
stepSnipe : Float -> Vector -> Action.Duration -> Champ -> (List String, Dict String Champ) -> (List String, Dict String Champ)
stepSnipe angle position (prevTicks, totalTicks) champ (log, dict) =
  let
    deltaTime =
      1/60
    arrowSpeed =
      10
    velocity =
      ( arrowSpeed * cos angle * deltaTime
      , arrowSpeed * sin angle * deltaTime
      )
    position' =
      Vector.add position velocity
      |> Debug.log "arrow position"
    enemies =
      dict
      -- Arrow cannot collide with self
      |> Dict.remove champ.name
      -- Arrow cannot collide with the dead
      |> Dict.filter (\_ enemy -> enemy.status /= Champ.Dead)
      |> Dict.values
  in
    case Collision.test enemies position' of
      Just (Collision.Enemy victim) ->
        let
          victim' =
            Champ.sufferDamage 75 victim
          -- HACK: since arrow is tied to champ.status, there's currently
          -- no way to keep arrow in flight after champ is cooled down.
          -- Need to decide how arrows should work. E.g. should ranger
          -- get stuck in snipe mode til arrow hits something?
          -- For now we teleport arrow out of world bounds.
          status' =
            Champ.ClassSpecific
              (Champ.Acting (Action.Snipe angle (-100, -100) (prevTicks + 1, totalTicks)))
          champ' =
            { champ
                | status = status'
            }
          log' =
            if victim'.status == Champ.Dead then
              (champ.name ++ " killed " ++ victim'.name ++ " with Snipe 🎯")
              :: log
            else
              log
        in
          dict
          -- Update the victim
          |> Dict.insert victim'.name victim'
          -- Update ranger
          |> Dict.insert champ.name champ'
          |> \dict -> (log', dict)
      -- Wall ->
      _ ->
        -- No collision yet, so keep moving arrow
        let
          duration' =
            (prevTicks + 1, totalTicks)
          status' =
            Champ.ClassSpecific
              (Champ.Acting (Action.Snipe angle position' duration'))
          champ' =
            { champ
                | status = status'
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
          Champ.Acting action ->
            case action of
              Action.Snipe angle arrow ((prevTick, totalTicks) as duration) ->
                -- Check if champ is done sniping
                if prevTick == totalTicks then
                  let
                    status' =
                      if List.isEmpty champ.waypoints then
                        Champ.Idling
                      else
                        Champ.Moving
                    champ' =
                      { champ
                          | status = status'
                      }
                  in
                    (log, Dict.insert champ'.name champ' dict)
                else
                  stepSnipe angle arrow duration champ (log, dict)
              _ ->
                Debug.crash "Unexpected classStatus"
          _ ->
            Debug.crash "Unexpected action"
      _ ->
        -- Idling | Moving, so no-op since these are handled outside of
        -- the class handler
        (log, dict)
