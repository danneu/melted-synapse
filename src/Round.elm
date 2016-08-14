

module Round exposing (..)


-- Elm
import Dict exposing (Dict)
import Array exposing (Array)
-- 1st
import Champ exposing (Champ)
import Vector


stepChamp : String -> Champ -> Dict String Champ -> Dict String Champ
stepChamp name champ dict =
  case champ.waypoints of
    [] ->
      dict
    waypoint :: rest ->
      let
      -- consume waypoint if champ is on it, else move champ towards it
      champ' =
        if Vector.dist champ.position waypoint.position < champ.speed * 1/60 then
          { champ
              | position = waypoint.position
              , waypoints = List.drop 1 champ.waypoints
          }
        else
          let
            (prevX, prevY) = champ.position
            -- TODO: Prevent champ from overshooting waypoint, then
            -- we can do length==0 comparison isntead of len<speed*timestep
            (dx, dy) =
              Vector.fromPoints champ.position waypoint.position
              |> Vector.normalize
              |> Vector.mult (champ.speed * 1/60)
            position' =
              (prevX + dx, prevY + dy)
            angle' =
              Vector.angleTo champ.position waypoint.position
            --_ = Debug.log "angle'" (angle', angle' * 180 / pi)
          in
            { champ
                | position = position'
                , angle = angle'

            }
      in
        Dict.insert name champ' dict


stepTick : Int -> List (Dict String Champ) -> List (Dict String Champ)
stepTick _  ticks =
  case ticks of
    [] ->
      Debug.crash "Impossible"
    prevChamps :: _ ->
      (Dict.foldl stepChamp prevChamps prevChamps) :: ticks


simulate : Dict String Champ -> Array (Dict String Champ)
simulate champs =
  List.foldl stepTick [champs] [1..180]
  --List.foldl stepTick [champs] [1..10]
  |> List.reverse
  |> Array.fromList
