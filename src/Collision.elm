

module Collision exposing (..)


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


-- position is the position of the thing that can collide with enemies
enemyCollision : Vector -> List Champ -> Maybe Champ
enemyCollision position enemies =
  let
    byDistance : Champ -> Float
    byDistance = \enemy ->
      Vector.dist position enemy.position
  in
    enemies
    |> List.sortBy byDistance
    |> List.head
    |> \maybeEnemy ->
         Maybe.andThen maybeEnemy
          ( \enemy ->
              if Vector.dist position enemy.position <= 1 then
                Just enemy
              else
                Nothing
          )


-- Callsite is responsible for filtering `champs` down to a list of
-- collidable champs.
test : List Champ -> Vector -> Maybe Collision
test champs position =
  case enemyCollision position champs of
    Just enemy ->
      Just (Enemy enemy)
    Nothing ->
      Nothing
