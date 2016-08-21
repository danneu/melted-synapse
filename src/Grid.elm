

module Grid exposing (..)


-- Elm
import Array exposing (Array)
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes
import Svg.Events
import Mouse
import Json.Decode as JD
import Dict exposing (Dict)
-- 1st
import Tile exposing (Tile)
import Waypoint exposing (Waypoint)
import Champ exposing (Champ)
import Constants exposing (tilesize)


type alias Grid =
  Array (Array Tile)


empty : Int -> Int -> Grid
empty cols rows =
  Array.repeat rows (Array.repeat cols Tile.empty)


-- VIEW


viewTile : Context msg -> Int -> Int -> Tile -> Svg msg
viewTile ctx y x tile =
  Svg.g
  [ Svg.Events.on "dblclick" (JD.succeed (ctx.onTileClick x y))
  ]
  [
    Svg.image
     [ Svg.Attributes.x (toString (toFloat x * tilesize))
     , Svg.Attributes.y (toString (toFloat y * tilesize))
     , Svg.Attributes.width <| toString tilesize
     , Svg.Attributes.height <| toString tilesize
     , let
         src =
           "./map/map_tile_" ++ toString x ++ "_" ++ toString y ++ ".png"
       in
         Svg.Attributes.xlinkHref src
     ]
     []
  , Svg.rect
    [ Svg.Attributes.width (toString tilesize)
    , Svg.Attributes.height (toString tilesize)
    , Svg.Attributes.x (toString (toFloat x * tilesize))
    , Svg.Attributes.y (toString (toFloat y * tilesize))
    , Svg.Attributes.stroke "black"
    , Svg.Attributes.fill "none"
    -- , Svg.Events.onMouseOut (ctx.onMouseOver x y)
    -- , Svg.Events.onMouseOver (ctx.onMouseOver x y)
    ]
    []
  , if ctx.showCoords then
      Svg.text'
        [ Svg.Attributes.x (toString (toFloat x * tilesize + tilesize / 4))
        , Svg.Attributes.y (toString (toFloat y * tilesize + tilesize / 2))
        , Svg.Attributes.class "no-select"
        , Svg.Attributes.fill "#ccb3c9"
        ]
        [ Svg.text <| toString (x, y) ]
    else
      Svg.text ""
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
    , ctx.onMouseDown
    ]
    [ Svg.g
      [ transform ]
      ( List.append
          (List.indexedMap (viewRow ctx) (Array.toList grid))
          ( let
              champCtx =
              { onChampClick = ctx.onChampClick
              , onWaypointClick = ctx.onWaypointClick
              , selectedChamp = ctx.selectedChamp
              , selectedWaypoint = ctx.selectedWaypoint
              , tickIdx = ctx.tickIdx
              }
            in
              ( List.map
                (Champ.view champCtx)
                -- Sort the dead champs first so they render behind alive champs
                (Champ.sortDeadFirst (Dict.values ctx.champs))
              )
          )
      )
    ]



type alias Context msg =
  { onTileClick : (Int -> Int -> msg)
  , onChampClick : (Champ -> msg)
  , onWaypointClick : (Champ -> Waypoint -> msg)
  , onMouseDown : Html.Attribute msg
  , offset : Mouse.Position
  , rows : Int
  , cols : Int
  , scale : Float
  , champs : Dict String Champ
  , selectedChamp : Maybe Champ
  , selectedWaypoint : Maybe Waypoint
  , showCoords : Bool
  , tickIdx : Int
  }
