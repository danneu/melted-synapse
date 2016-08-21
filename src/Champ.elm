

module Champ exposing (..)


-- Elm
import Svg exposing (..)
import Svg.Attributes
import Svg.Events
import String
-- 1st
import Waypoint exposing (Waypoint)
import Vector exposing (Vector)
import Constants exposing (tilesize)
import Action exposing (Action)


type Status
  -- Champ is just standing there (no waypoints)
  = Idling
  -- Champ is on the move towards its next waypoint
  | Moving
  -- Champ is in the middle of its autoattack animation
  -- Holds the current tick and the total tick count of the status
  -- and also holds the target champ
  | AutoAttacking (Int, Int) Champ
  | Dead


type alias Champ =
  { name : String
    -- roundDelta stores the champ's change in health for the current round.
    -- It's displayed in the health bar and should be reset before every round.
  , hp : (Int, Int, Int) -- (currentHp, maxHp, roundDelta)
  , position : (Float, Float)  -- x, y
  , waypoints : List Waypoint
  , speed : Float -- meters aka tiles per second
  , angle : Float
  , status : Status
  }


-- Convenience function for creating champs
init : String -> Vector -> (Int, Int) -> Champ
init name position (currHp, maxHp) =
  { name = name
  , status = Idling
  , hp = (currHp, maxHp, 0)
  , position = position
  , speed = 2
  , angle = 0
  , waypoints = []
  }


addWaypoint : Vector -> List Action -> Champ -> Champ
addWaypoint position actions champ =
  let
    waypoint =
      Waypoint.make position actions
  in
    { champ
        | waypoints = List.append champ.waypoints [waypoint]
        , status = Moving
    }
    |> faceWaypoint


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


-- Makes champ face its attack victim if there is one
faceVictim : Champ -> Champ
faceVictim champ =
  case champ.status of
    AutoAttacking _ enemy ->
      { champ
          | angle = Vector.angleTo champ.position enemy.position
      }
    _ ->
      champ


-- Sorting the dead champs first lets us render them behind the alive champs.
sortDeadFirst : List Champ -> List Champ
sortDeadFirst =
  let
    compare =
      \a b ->
        case (a.status, b.status) of
          (Dead, Dead) ->
            EQ
          (Dead, _) ->
            LT
          (_, Dead) ->
            GT
          (_, _) ->
            EQ
  in
    List.sortWith compare


-- I think something higher level should be doing (affect champ1 champ2 attackType)
sufferDamage : Int -> Champ -> Champ
sufferDamage damage champ =
  let
    (currHp, maxHp, delta) =
      champ.hp
    currHp' =
      currHp - damage
    status' =
      if currHp' <= 0 then
        Dead
      else
        champ.status
  in
    { champ
        | hp = (currHp', maxHp, max (delta - currHp) (delta - damage))
        , status = status'
    }


-- Called at the start of every simulation round since some state does
-- not carry from round to round.
-- TODO: Probably means this state should live in Round.elm.
roundReset : Champ -> Champ
roundReset champ =
  let
    (currHp, maxHp, _) = champ.hp
  in
    { champ
        | hp = (currHp, maxHp, 0)
    }


-- VIEW


statusToEmoji : Status -> String
statusToEmoji status =
  case status of
    Idling ->
      "⌛"
    Moving ->
      "⏩️"
    AutoAttacking _ _ ->
      "⚔"
    Dead ->
      "⚰"


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
          , Svg.Attributes.strokeOpacity
              (if champIsSelected then "1.0" else "0.3")
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
        , let
            degrees =
              -- Don't rotate the tombstone graphic
              if champ.status == Dead then
                0
              else
                (champ.angle * 180 / pi) + 90
            (originX, originY) =
              (x * tilesize + tilesize / 2, y * tilesize + tilesize / 2)
            transform =
              "rotate("
              ++ String.join " " (List.map toString [degrees, originX, originY])
              ++ ")"
            imageSrc =
              -- TODO: DRY or extract
              -- animSpeed 1.0 means play one loop of the animation per round
              case champ.status of
                AutoAttacking (curr, duration) _ ->
                  let
                    animSpeed =
                      1.0
                    frames =
                      9 -- frame count of animation
                    bucket =
                      floor (toFloat (curr - 1) / (toFloat duration / frames / animSpeed)) % frames
                  in
                    "./img/sprites/champ/attack_" ++ toString bucket ++ ".png"
                Moving ->
                  let
                    animSpeed =
                      4.0
                    frames =
                      17
                    bucket =
                      floor (toFloat ctx.tickIdx / (toFloat Constants.ticksPerRound / frames / animSpeed)) % frames
                  in
                    "./img/sprites/champ/move_" ++ toString bucket ++ ".png"
                Idling ->
                  let
                    animSpeed =
                      2.0
                    frames =
                      17
                    bucket =
                      floor (toFloat ctx.tickIdx / (toFloat Constants.ticksPerRound / frames / animSpeed)) % frames
                  in
                    "./img/sprites/champ/idle_" ++ toString bucket ++ ".png"
                Dead ->
                  "./img/tombstone.png"
            -- Scale the champ image to 128x128 instead of 64x64
            -- unless they are dead
            (x', y', side) =
              if champ.status == Dead then
                ( x * tilesize
                , y * tilesize
                , tilesize
                )
              else
                ( x * tilesize - tilesize / 2
                , y * tilesize - tilesize / 2
                , tilesize * 2
                )

          in
            Svg.image
            [ Svg.Attributes.x (toString x')
            , Svg.Attributes.y (toString y')
            , Svg.Attributes.width <| toString side
            , Svg.Attributes.height <| toString side
            , Svg.Attributes.xlinkHref imageSrc
            , Svg.Attributes.transform transform
            , Svg.Events.onClick (ctx.onChampClick champ)
            ]
            []
        -- Show champ name and HP bar
        , let
            marginTop = -10
            fullHeight = 6
            fullWidth = tilesize
            (currHp, maxHp, deltaHp) = champ.hp
            currWidth =
              (tilesize * (toFloat currHp / toFloat maxHp))
              |> round
              |> min (round tilesize)
              |> max 0
            deltaWidth =
              (tilesize * (toFloat (max 0 currHp + abs deltaHp) / toFloat maxHp))
              |> round
              |> min (round tilesize)
              |> max 0
          in
            Svg.g
            []
            [
              -- name
              Svg.text'
              [ Svg.Attributes.x (toString (x * tilesize))
              , Svg.Attributes.y (toString (y * tilesize + marginTop - 5))
              , Svg.Attributes.fill "white"
              , Svg.Attributes.class "no-select"
              ]
              [ Svg.text (champ.name ++ " " ++ statusToEmoji champ.status)
              ]
              -- background
            , Svg.rect
              [ Svg.Attributes.x (toString (x * tilesize))
              , Svg.Attributes.y (toString (y * tilesize + marginTop))
              , Svg.Attributes.width <| toString fullWidth
              , Svg.Attributes.height <| toString fullHeight
              , Svg.Attributes.fill "black"
              , Svg.Attributes.stroke "black"
              , Svg.Attributes.strokeWidth "3"
              ]
              []
              -- foreground (deltaHp)
            , Svg.rect
              [ Svg.Attributes.x (toString (x * tilesize))
              , Svg.Attributes.y (toString (y * tilesize + marginTop))
              , Svg.Attributes.width <| toString deltaWidth
              , Svg.Attributes.height <| toString fullHeight
              , Svg.Attributes.fill
                   (if deltaHp < 0 then "red" else "#00ff00")
              ]
              []
              -- foreground (currHp)
            , Svg.rect
              [ Svg.Attributes.x (toString (x * tilesize))
              , Svg.Attributes.y (toString (y * tilesize + marginTop))
              , Svg.Attributes.width <| toString currWidth
              , Svg.Attributes.height <| toString fullHeight
              , Svg.Attributes.fill "#0000ff"
              ]
              []
            ]
        ]
        ( List.map
            (viewWaypoint ctx.selectedWaypoint (ctx.onWaypointClick champ))
            champ.waypoints
        )
      )


type alias Context msg =
  { onChampClick : (Champ -> msg)
  , onWaypointClick : (Champ -> Waypoint -> msg)
  , selectedChamp : Maybe Champ
  , selectedWaypoint : Maybe Waypoint
  , tickIdx : Int
  }
