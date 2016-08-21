

module Class exposing (..)


type Class
  = Warrior
  | Ranger


toEmoji : Class -> String
toEmoji class =
  case class of
    Warrior ->
      "⚔"
    Ranger ->
      "🏹"
