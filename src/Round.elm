

module Round exposing (..)


-- Elm
import Dict exposing (Dict)
import Array exposing (Array)
-- 1st
import Champ exposing (Champ)


stepChamp : String -> Champ -> Dict String Champ -> Dict String Champ
stepChamp name champ dict =
  case champ.waypoints of
    [] ->
      dict
    waypoint :: _ ->
      let
        (prevX, prevY) = champ.position
        position' =
          (prevX + champ.speed * 1/60, prevY + champ.speed * 1/60)
        champ' =
          { champ
              | position = position'
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
  --List.reverse (List.foldl stepTick [champs] [1..180])
  List.foldl stepTick [champs] [1..10]
  |> List.reverse
  |> Array.fromList
