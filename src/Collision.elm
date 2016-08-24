

module Collision exposing (..)


import Dict exposing (Dict)
-- 1st
import Champ exposing (Champ)
import Vector exposing (Vector)


-- Another idea:
--
-- Collision.test world model'
-- |> Maybe.map (handleCollision model')
-- |> Maybe.withDefault model

-- Or maybe Collision returns the updated entity position
-- and a possible collision result, together.

-- FIXME: The code in this module is nasty.


type Collision
  = Wall
  | Enemy Champ


enemyCollision : Champ -> List Champ -> Maybe Champ
enemyCollision champ enemies =
  let
    byDistance : Champ -> Float
    byDistance = \enemy ->
      Vector.dist champ.position enemy.position
  in
    enemies
    |> List.sortBy byDistance
    |> List.head
    |> \maybeEnemy ->
         Maybe.andThen maybeEnemy
          ( \enemy ->
              if Vector.dist champ.position enemy.position <= 1 then
                Just enemy
              else
                Nothing
          )


test : Dict String Champ -> Champ -> Maybe Collision
test dict champ =
  let
    otherChamps =
      dict
      -- Cannot collide with self
      |> Dict.remove champ.name
      -- Cannot collide with the dead
      |> Dict.filter (\_ enemy -> enemy.status /= Champ.Dead)
      |> Dict.values
  in
    case enemyCollision champ otherChamps of
      Just enemy ->
        Just (Enemy enemy)
      Nothing ->
        Nothing
