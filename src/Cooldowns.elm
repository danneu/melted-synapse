

module Cooldowns exposing (..)


import Dict exposing (Dict)
-- 3rd
import AllDict exposing (AllDict)
-- 1st
import Class exposing (Class)
import Action exposing (Action)


ord : Action -> Int
ord action =
  case action of
    -- GENERAL
    Action.Wait _ ->
      0
    -- WARRIOR
    Action.Charge _ ->
      1
    -- RANGER
    Action.Snipe _ _ _ ->
      2


duration : Action -> Int
duration action =
  case action of
    -- GENERAL
    Action.Wait _ ->
      0
    -- WARRIOR
    Action.Charge _ ->
      60
    -- RANGER
    Action.Snipe _ _ _ ->
      60


-- The value of each cooldown key is (Maybe currTick, totalTicks)
-- (Just currTick) means that the action is cooling down, Nothing means
-- it is ready to use. 60 ticks = 1 second
-- totalTicks == 0 can be used to establish that there is no cooldown for the act
type alias Cooldowns =
  AllDict Action (Maybe Int, Int) Int
  --Dict String (Maybe Int, Int)


-- REVISIT: Couldn't get this working using Actions as keys with AnyDict
-- since I can't just specify an action tag, rather I'd have to
-- construct an Action with dummy values which seems worse than
-- using strings like this so far.
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
          [ let ability = Action.Charge 0 in
              (ability, (Nothing, duration ability))
          ]
        Class.Ranger ->
          [ let ability = Action.Snipe 0 (0, 0) (0, 0) in
              (ability, (Nothing, duration ability))
          ]
  in
    AllDict.fromList ord (List.append generalAbilities classAbilities)


tick : Cooldowns -> Cooldowns
tick cooldowns =
  let
    tick1 = \ability (maybePrevTick, totalTicks) ->
      case maybePrevTick of
        Nothing ->
          -- Already cooled
          (Nothing, totalTicks)
        Just prevTick ->
          if prevTick == totalTicks then
            -- Just cooled
            (Nothing, totalTicks)
          else
            -- Still cooling
            (Just (prevTick + 1), totalTicks)
  in
    AllDict.map tick1 cooldowns


isCool : Cooldowns -> Action -> Bool
isCool cooldowns action =
  case AllDict.get action cooldowns of
    Nothing ->
      False
    Just (maybePrevTick, _) ->
      maybePrevTick == Nothing


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
