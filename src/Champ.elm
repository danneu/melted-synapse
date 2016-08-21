

module Champ exposing (..)


-- Elm
import Svg exposing (..)
import Svg.Attributes
import Svg.Events
import String
import Set exposing (Set)
-- 1st
import Waypoint exposing (Waypoint)
import Vector exposing (Vector)


type Action
  -- Champ is just standing there (no waypoints)
  = Idling
  -- Champ is on the move towards its next waypoint
  | Moving
  -- Champ is in the middle of its autoattack animation
  -- Holds the current tick and the total tick count of the action
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
    -- Holds set of enemy champ names that this champ has autoattacked in
    -- the current round to ensure that a champ only autoattacks another champ
    -- once per round.
  , autoattacked : Set String
  , action : Action
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


-- Makes champ face its attack victim if there is one
faceVictim : Champ -> Champ
faceVictim champ =
  case champ.action of
    AutoAttacking _ enemy ->
      { champ
          | angle = Vector.angleTo champ.position enemy.position
      }
    _ ->
      champ


sortDeadFirst : List Champ -> List Champ
sortDeadFirst =
  let
    compare =
      \a b ->
        case (a.action, b.action) of
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


sufferDamage : Int -> Champ -> Champ
sufferDamage damage champ =
  let
    (currHp, maxHp, delta) =
      champ.hp
    currHp' =
      currHp - damage
    action' =
      if currHp' <= 0 then
        Dead
      else
        champ.action
  in
    { champ
        | hp = (currHp', maxHp, max -currHp -damage)
        , action = action'
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
        -- the autoattacked set only exists to ensure a champ only attacks
        -- another enemy once per round, so it gets cleared each round.
        | autoattacked = Set.empty
        , hp = (currHp, maxHp, 0)
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
              -- Don't rotate the tombstone graphic
              if champ.action == Dead then
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
              case champ.action of
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
                      floor (toFloat ctx.tickIdx / (toFloat ctx.ticksPerRound / frames / animSpeed)) % frames
                  in
                    "./img/sprites/champ/move_" ++ toString bucket ++ ".png"
                Idling ->
                  let
                    animSpeed =
                      2.0
                    frames =
                      17
                    bucket =
                      floor (toFloat ctx.tickIdx / (toFloat ctx.ticksPerRound / frames / animSpeed)) % frames
                  in
                    "./img/sprites/champ/idle_" ++ toString bucket ++ ".png"
                Dead ->
                  "./img/tombstone.png"
          in
            -- Scale the champ image to 128x128 instead of 64x64
            Svg.image
            [ Svg.Attributes.x (toString (x * tilesize - 64/2))
            , Svg.Attributes.y (toString (y * tilesize - 64/2))
            , Svg.Attributes.width <| toString (tilesize * 2)
            , Svg.Attributes.height <| toString (tilesize * 2)
            -- , Svg.Attributes.width <| toString tilesize
            -- , Svg.Attributes.height <| toString tilesize
            , Svg.Attributes.xlinkHref imageSrc
            , Svg.Attributes.transform transform
            , Svg.Events.onClick (ctx.onChampClick champ)
            ]
            []
        -- Show champ's current action
        , let
            text =
              case champ.action of
                Idling ->
                  "Idling"
                Moving ->
                  "Moving"
                AutoAttacking (curr, duration) _ ->
                  "Attacking (" ++ toString curr ++ ", " ++ toString duration ++ ")"
                Dead ->
                  ""
          in
            Svg.text'
              [ Svg.Attributes.x (toString (x * tilesize + tilesize / 4))
              , Svg.Attributes.y (toString (y * tilesize + tilesize / 1))
              , Svg.Attributes.class "no-select"
              , Svg.Attributes.fill "white"
              ]
              [ Svg.text text ]
        -- Show champ name and HP bar
        , let
            marginTop = -10
            fullHeight = 6
            fullWidth = tilesize
            (currHp, maxHp, deltaHp) = champ.hp
            currWidth =
              (round (tilesize * (toFloat currHp / toFloat maxHp)))
              |> min (round tilesize)
              |> max 0
            deltaWidth =
              --(toFloat currWidth) + abs (toFloat deltaHp) - (padding / 2)
              (round (tilesize * (toFloat (max 0 currHp + abs deltaHp) / toFloat maxHp)))
              |> min (round tilesize)
              |> max 0
            --_ = Debug.log "currW, delatW" (currWidth, deltaWidth)
          in
            Svg.g
            []
            [
              -- name
              Svg.text'
              [ Svg.Attributes.x (toString (x * tilesize))
              , Svg.Attributes.y (toString (y * tilesize + marginTop - 5))
              , Svg.Attributes.fill "white"
              ]
              [ Svg.text
                  <| champ.name
                     ++ if champ.action == Dead then " (dead)" else ""
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
        (List.map (viewWaypoint ctx.selectedWaypoint (ctx.onWaypointClick champ)) champ.waypoints)
      )


type alias Context msg =
  { onChampClick : (Champ -> msg)
  , onWaypointClick : (Champ -> Waypoint -> msg)
  , selectedChamp : Maybe Champ
  , selectedWaypoint : Maybe Waypoint
  , tickIdx : Int
  , ticksPerRound : Int
  }
