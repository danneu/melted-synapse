

module Grid exposing (..)


-- Elm
import Array exposing (Array)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Svg exposing (..)
import Svg.Attributes
import Svg.Events
import Mouse
import String
-- 1st
import Tile exposing (Tile)
import Waypoint exposing (Waypoint)
import Champ exposing (Champ)


type alias Grid =
  Array (Array Tile)


tilesize : Int
tilesize =
  64


empty : Int -> Int -> Grid
empty cols rows =
  Array.repeat rows (Array.repeat cols Tile.empty)


-- VIEW


viewTile : Context msg -> Int -> Int -> Tile -> Svg msg
viewTile ctx y x tile =
  Svg.g
  []
  [
    Svg.image
     [ Svg.Attributes.x (toString (x * tilesize))
     , Svg.Attributes.y (toString (y * tilesize))
     , Svg.Attributes.width <| toString tilesize
     , Svg.Attributes.height <| toString tilesize
     , Svg.Attributes.xlinkHref "/img/grass1.gif"
     , Svg.Events.onClick (ctx.onTileClick x y)
     ]
     []
  , Svg.rect
    [ Svg.Attributes.width (toString tilesize)
    , Svg.Attributes.height (toString tilesize)
    , Svg.Attributes.x (toString (x * tilesize))
    , Svg.Attributes.y (toString (y * tilesize))
    , Svg.Attributes.stroke "black"
    , Svg.Attributes.fill "none"
    --, Svg.Attributes.fillOpacity fillOpacity
    --, Svg.Attributes.class "tile"
    -- , Svg.Events.onMouseOut (ctx.onMouseOver x y)
    -- , Svg.Events.onMouseOver (ctx.onMouseOver x y)
    , Svg.Events.onClick (ctx.onTileClick x y)
    ]
    []
  , Svg.text'
      [ Svg.Attributes.x (toString (x * tilesize + tilesize // 4))
      , Svg.Attributes.y (toString (y * tilesize + tilesize // 2))
      ]
      [ Svg.text <| toString (x, y) ]
  , if List.any (\wp -> wp.position == (x, y)) ctx.champ.waypoints then
      Svg.image
      [ Svg.Attributes.x (toString (x * tilesize))
      , Svg.Attributes.y (toString (y * tilesize))
      , Svg.Attributes.width <| toString tilesize
      , Svg.Attributes.height <| toString tilesize
      , Svg.Attributes.xlinkHref "/img/waypoint.png"
      ]
      []
    else
      Svg.text' [] [ Svg.text "" ]
  , if ctx.champ.position == (x, y) then
      Svg.image
      [ Svg.Attributes.x (toString (x * tilesize))
      , Svg.Attributes.y (toString (y * tilesize))
      , Svg.Attributes.width <| toString tilesize
      , Svg.Attributes.height <| toString tilesize
      , Svg.Attributes.xlinkHref "/img/warrior128.png"
      ]
      []
    else
      Svg.text' [] [ Svg.text "" ]
  ]



viewRow : Context msg -> Int -> Array Tile -> Svg msg
viewRow ctx y row =
  Svg.g
  []
  (List.indexedMap (viewTile ctx y) (Array.toList row))



view : Context msg -> Grid -> Html msg
view ctx grid =
  let
    translate =
      "translate("
      ++ toString ctx.offset.x ++ " "
      ++ toString ctx.offset.y
      ++ ") "
      ++ "scale(" ++ toString ctx.scale ++ ")"
    transform =
      Svg.Attributes.transform translate
  in
    Svg.svg
    [ Svg.Attributes.class "tile-map"
    --, ctx.onMouseDown
    , Html.Attributes.style
        [ ( "background-position",
            (toString <| ctx.offset.x // -2) ++ "px "
            ++ (toString <| ctx.offset.y // -2) ++ "px"
          )
        ]
    ]
    [ Svg.g
      [ transform ]
      (List.indexedMap (viewRow ctx) (Array.toList grid))
    , Svg.polyline
      [ Svg.Attributes.fill "none"
      , Svg.Attributes.stroke "black"
      , Svg.Attributes.strokeWidth "6"
      , let
          coords = [ (4, 4), (7, 1), (9, 6)]
          points =
            List.map
              (\ (x, y) ->
                 ( x * tilesize + tilesize // 2
                 , y * tilesize + tilesize // 2
                 )
              )
              coords
          string =
            (List.map (\ (x, y) -> toString x ++ "," ++ toString y) points)
            |> String.join " "
        in
          Svg.Attributes.points string
      ]
      []
    ]



type alias Context msg =
  { onTileClick : (Int -> Int -> msg)
    --, onMouseDown : Html.Attribute msg
    --, onMouseOver : (Int -> Int -> msg)
  , offset : Mouse.Position
  , rows : Int
  , cols : Int
  , scale : Float
  , champ : Champ
  }
