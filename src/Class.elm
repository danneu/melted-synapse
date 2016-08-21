

module Class exposing (..)


-- 1st
import Action


type Class
  = Warrior (Maybe Action.WarriorAction)
  | Ranger (Maybe Action.RangerAction)


toEmoji : Class -> String
toEmoji class =
  case class of
    Warrior _ ->
      "⚔"
    Ranger _ ->
      "🏹"
