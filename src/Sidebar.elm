

module Sidebar exposing (..)


-- Elm
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.App
-- 1st
import Waypoint exposing (Waypoint)
import Champ exposing (Champ)
import WaypointDetail
import ChampDetail
import Action


-- MODEL


type Detail
  = None
  | ChampDetail Champ
  | WaypointDetail WaypointDetail.Model


type alias Model =
  { detail : Detail
  }


init : Model
init =
  { detail = None
  }


-- UPDATE


type OutMsg
  = NoOp
  | WaypointDetailOutMsg WaypointDetail.OutMsg


type Msg
  = Clear
  -- CHILDREN
  | WaypointSelected String Waypoint
  | ChampSelected Champ
  | WaypointDetailMsg WaypointDetail.Msg
  | ChampDetailMsg ChampDetail.Msg


update : Msg -> Model -> (Model, OutMsg)
update msg model =
  case (msg, model.detail) of
    (Clear, _) ->
      (init, NoOp)
    --
    -- WAYPOINT
    --
    (WaypointSelected champName waypoint, _) ->
      ( { model
            | detail = WaypointDetail (WaypointDetail.init champName waypoint)
        }
      , NoOp
      )
    (WaypointDetailMsg childMsg, WaypointDetail childModel) ->
      let
        (childModel', childOutMsg) =
          WaypointDetail.update childMsg childModel
      in
      ( { model
            | detail = WaypointDetail childModel'
        }
      , WaypointDetailOutMsg childOutMsg
      )
    --
    -- CHAMP
    --
    (ChampSelected champ, _) ->
      ( { model
            | detail = ChampDetail (ChampDetail.init champ)
        }
      , NoOp
      )
    (ChampDetailMsg childMsg, ChampDetail childModel) ->
      let
        (childModel', childOutMsg) =
          ChampDetail.update childMsg childModel
        detail' =
          case childOutMsg of
            ChampDetail.SelectWaypoint waypoint ->
              WaypointDetail (WaypointDetail.init childModel'.name waypoint)
      in
      ( { model
            | detail = detail'
        }
      , NoOp
      )
    _ ->
      (model, NoOp)



-- VIEW


view : Model -> Html Msg
view model =
  if model.detail == None then
    Html.text ""
  else
    Html.div
    [ Html.Attributes.id "sidebar" ]
    [ case model.detail of
        WaypointDetail childModel ->
          Html.App.map WaypointDetailMsg (WaypointDetail.view childModel)
        ChampDetail champ ->
          Html.App.map ChampDetailMsg (ChampDetail.view champ)
        _ ->
          Html.text ""
    -- X button
    , Html.button
      [ Html.Events.onClick Clear
      , Html.Attributes.style
          [ ("position", "absolute")
          , ("top", "0")
          , ("right", "0")
          ]
      ]
      [ Html.text "X" ]
    ]
