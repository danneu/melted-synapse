

module Champ exposing (..)


-- Elm
import Svg exposing (..)
import Svg.Attributes
import Svg.Events
import String
-- 1st
import Waypoint exposing (Waypoint)


type alias Champ =
  { name : String
  , hp : (Int, Int) -- (currentHp, maxHp)
  , position : (Float, Float)  -- x, y
  , waypoints : List Waypoint
  , speed : Float -- meters aka tiles per second
  , angle : Float
  }


tilesize : Float
tilesize =
  64


viewWaypoint : (Waypoint -> msg) -> Waypoint -> Svg msg
viewWaypoint onWaypointClick waypoint =
  let
    (x, y) = waypoint.position
  in
    Svg.image
    [ Svg.Attributes.x (toString (x * tilesize))
    , Svg.Attributes.y (toString (y * tilesize))
    , Svg.Attributes.width <| toString tilesize
    , Svg.Attributes.height <| toString tilesize
    , Svg.Attributes.xlinkHref "/img/waypoint.png"
    , Svg.Events.onClick (onWaypointClick waypoint)
    ]
    []


view : Context msg -> Champ -> Svg msg
view ctx champ =
  let
    (x, y) = champ.position
  in
    Svg.g
    []
    ( List.append
        [ Svg.polyline
          [ Svg.Attributes.fill "none"
          , Svg.Attributes.stroke "black"
          , Svg.Attributes.strokeWidth "6"
          , let
              points =
                List.map
                  (\ (x, y) ->
                    ( x * tilesize + tilesize / 2
                    , y * tilesize + tilesize / 2
                    )
                  )
                  (champ.position :: (List.map .position champ.waypoints))
              string =
                (List.map (\ (x, y) -> toString x ++ "," ++ toString y) points)
                |> String.join " "
            in
              Svg.Attributes.points string
          ]
          []
        , let
            degrees =
              (champ.angle * 180 / pi) + 90
            (originX, originY) =
              (x * tilesize + tilesize / 2, y * tilesize + tilesize / 2)
            transform =
              "rotate("
              ++ String.join " " (List.map toString [degrees, originX, originY])
              ++ ")"
          in
            Svg.image
            [ Svg.Attributes.x (toString (x * tilesize))
            , Svg.Attributes.y (toString (y * tilesize))
            , Svg.Attributes.width <| toString tilesize
            , Svg.Attributes.height <| toString tilesize
            , Svg.Attributes.xlinkHref "/img/warrior128.png"
            , Svg.Attributes.transform transform
            , Svg.Events.onClick (ctx.onChampClick champ)
            ]
            []
        ]
        (List.map (viewWaypoint (ctx.onWaypointClick champ)) champ.waypoints)
      )


type alias Context msg =
  { onChampClick : (Champ -> msg)
  , onWaypointClick : (Champ -> Waypoint -> msg)
  }
