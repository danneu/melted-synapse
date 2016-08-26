

module Cooldowns exposing (..)


import Dict exposing (Dict)
-- 3rd
import AllDict exposing (AllDict)
-- 1st
import Class exposing (Class)
import Action exposing (Action)


-- Tells AllDict how to turn an Action into a key.
ord : Action -> Int
ord action =
  case action of
    -- GENERAL
    Action.Wait _ ->
      0
    -- WARRIOR
    Action.Charge _ _ ->
      1
    -- RANGER
    Action.Snipe _ _ _ ->
      2


-- Lets other modules query the cooldown duration of an action.
duration : Action -> Int
duration action =
  case action of
    -- GENERAL
    Action.Wait _ ->
      0
    -- WARRIOR
    Action.Charge _ _ ->
      60
    -- RANGER
    Action.Snipe _ _ _ ->
      60


-- The value of each cooldown key is (Maybe elaspedTicks, totalTicks)
--   - (Just elapsed) means that the action is cooling down
--   - Nothing means it is ready to use.
-- 60 ticks = 1 second
-- totalTicks == 0 can be used to establish that there is no cooldown for the act
type alias Cooldown =
  (Maybe Int, Int)


type alias Cooldowns =
  AllDict Action Cooldown Int


-- This looks weird because I needed to construct dummy Actions to
-- too use my ord/duration functions.
init : Class -> Cooldowns
init class =
  let
    generalAbilities =
      [ let ability = Action.Wait (0, 0) in
          (ability, (Nothing, duration ability))
      ]
    classAbilities =
      case class of
        Class.Warrior ->
          [ let ability = Action.Charge 0 (0, 0) in
              (ability, (Nothing, duration ability))
          ]
        Class.Ranger ->
          [ let ability = Action.Snipe 0 (0, 0) (0, 0) in
              (ability, (Nothing, duration ability))
          ]
  in
    AllDict.fromList ord (List.append generalAbilities classAbilities)


-- Cools down every ability in the dict by one tick
tick : Cooldowns -> Cooldowns
tick cooldowns =
  let
    tick1 = \ability (maybe, totalTicks) ->
      case maybe of
        Nothing ->
          -- Already cooled
          (Nothing, totalTicks)
        Just elapsed ->
          if elapsed == totalTicks then
            -- Just cooled
            (Nothing, totalTicks)
          else
            -- Still cooling
            (Just (elapsed + 1), totalTicks)
  in
    AllDict.map tick1 cooldowns


-- Checks if an action is cooled down and ready to be used again.
isCool : Cooldowns -> Action -> Bool
isCool cooldowns action =
  case AllDict.get action cooldowns of
    Nothing ->
      False
    Just (maybe, _) ->
      maybe == Nothing


-- Begin cooling down an ability for its totalTicks duration.
-- Use this any time a champ uses an ability.
heatUp : Cooldowns -> Action -> Cooldowns
heatUp cooldowns action =
  AllDict.update
    action
    (\maybe ->
       case maybe of
         Nothing ->
           Debug.crash "Trying to heat up an unexpected ability"
         Just (_, totalTicks) ->
           Just (Just 0, totalTicks)
    )
    cooldowns
