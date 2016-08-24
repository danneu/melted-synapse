

module Action exposing (..)


import Html exposing (Html)
import Html.Attributes
-- 3rd
import Numeral
-- 1st
import Util


--
-- Actions are things that a champ can enqueue at a Waypoint
--


type alias Angle =
  Float


-- Starts at (1, tickDuration), ends at (tickDuration, tickDuration)
type alias Duration =
  (Int, Int)


-- TODO: Will there be General actions not specific to a class?
type Action
  -- WARRIOR
  = Charge Angle
  --| WarCry Angle Duration
  -- RANGER
  | Snipe Angle Duration
  -- GENERAL
  | Wait Duration



-- User-friendly name of the action
toHtml : Action -> Html msg
toHtml action =
  case action of
    Wait (_, totalTicks) ->
      let
        seconds =
          toFloat totalTicks / 60
        suffix =
          if seconds == 1 then " second" else " seconds"
      in
        Html.text ("Wait " ++ Numeral.format "0[.]0" seconds ++ suffix)
    Charge angle ->
      let
        degrees =
          round (Util.toDegrees angle)
      in
        Html.span
        []
        [ Html.text "Charge "
        , Html.span
          [ Html.Attributes.style
              [ ("display", "inline-block")
              , ("transform", "rotate(" ++ Basics.toString degrees ++ "deg)")
              ]
          ]
          [ Html.text "→" ]
        ]
    Snipe angle _ ->
      let
        degrees =
          round (Util.toDegrees angle)
      in
        Html.text
        <| "Snipe " ++ Basics.toString degrees ++ "°"