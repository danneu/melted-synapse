

module Champ exposing (..)


-- Elm
import Svg exposing (..)
import Svg.Attributes
import Svg.Events
import String
-- 1st
import Waypoint exposing (Waypoint)
import Vector


type alias Champ =
  { name : String
  , hp : (Int, Int) -- (currentHp, maxHp)
  , position : (Float, Float)  -- x, y
  , waypoints : List Waypoint
  , speed : Float -- meters aka tiles per second
  , angle : Float
  }


-- sets champ's angle based on next waypoint
-- noops if no waypoint
--
-- Call this whenever the first waypoint is updated
faceWaypoint : Champ -> Champ
faceWaypoint champ =
  case champ.waypoints of
    [] ->
      champ
    waypoint :: _ ->
      { champ
          | angle = Vector.angleTo champ.position waypoint.position
      }


-- VIEW


tilesize : Float
tilesize =
  64


viewWaypoint : Maybe Waypoint -> (Waypoint -> msg) -> Waypoint -> Svg msg
viewWaypoint maybeWaypoint onWaypointClick waypoint =
  let
    (x, y) = waypoint.position
    -- TEMP HACK: waypoint positions are not going to be unique
    -- since they can stack. Need a better way to compare waypoints.
    isSelected =
      case maybeWaypoint of
        Just selectedWaypoint ->
          selectedWaypoint.position == waypoint.position
        _ ->
          False
  in
    Svg.g
    []
    [ if isSelected then
        Svg.rect
        [ Svg.Attributes.x (toString (x * tilesize))
        , Svg.Attributes.y (toString (y * tilesize))
        , Svg.Attributes.width <| toString tilesize
        , Svg.Attributes.height <| toString tilesize
        , Svg.Attributes.fill "none"
        , Svg.Attributes.stroke "cyan"
        , Svg.Attributes.strokeWidth "3"
        ]
        []
      else
        Svg.text' [] []
    , Svg.image
      [ Svg.Attributes.x (toString (x * tilesize))
      , Svg.Attributes.y (toString (y * tilesize))
      , Svg.Attributes.width <| toString tilesize
      , Svg.Attributes.height <| toString tilesize
      , Svg.Attributes.xlinkHref "./img/waypoint.png"
      , Svg.Events.onClick (onWaypointClick waypoint)
      ]
      []
    ]


view : Context msg -> Champ -> Svg msg
view ctx champ =
  let
    champIsSelected =
      case ctx.selectedChamp of
        Just selectedChamp ->
          selectedChamp.name == champ.name
        _ ->
          False
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
        -- This circle shows auto-attack range
        , if champIsSelected then
            Svg.circle
            [ Svg.Attributes.cx (toString (x * tilesize + tilesize / 2))
            , Svg.Attributes.cy (toString (y * tilesize + tilesize / 2))
            , Svg.Attributes.r (toString tilesize)
            -- , Svg.Attributes.fill "red"
            -- , Svg.Attributes.fillOpacity "0.5"
            , Svg.Attributes.fill "none"
            , Svg.Attributes.stroke "red"
            , Svg.Attributes.strokeWidth "5"
            , Svg.Attributes.strokeDasharray "10,5"
            , Svg.Attributes.strokeOpacity "0.75"
            ]
            []
          else
            Svg.text' [] []
        -- General selection indicator
        -- , if champIsSelected then
        --     Svg.rect
        --     [ Svg.Attributes.x (toString (x * tilesize))
        --     , Svg.Attributes.y (toString (y * tilesize))
        --     , Svg.Attributes.width <| toString tilesize
        --     , Svg.Attributes.height <| toString tilesize
        --     , Svg.Attributes.fill "none"
        --     , Svg.Attributes.stroke "yellow"
        --     , Svg.Attributes.strokeWidth "3"
        --     ]
        --     []
        --   else
        --     Svg.text' [] []
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
            , Svg.Attributes.xlinkHref "./img/champ-idle.gif"
            , Svg.Attributes.transform transform
            , Svg.Events.onClick (ctx.onChampClick champ)
            ]
            []
        ]
        (List.map (viewWaypoint ctx.selectedWaypoint (ctx.onWaypointClick champ)) champ.waypoints)
      )


type alias Context msg =
  { onChampClick : (Champ -> msg)
  , onWaypointClick : (Champ -> Waypoint -> msg)
  , selectedChamp : Maybe Champ
  , selectedWaypoint : Maybe Waypoint
  }
