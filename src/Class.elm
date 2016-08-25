

module Class exposing (..)


type Class
  = Warrior
  | Ranger


toIcon : Class -> String
toIcon class =
  case class of
    Warrior ->
      "⚔"
    Ranger ->
      "🏹"
