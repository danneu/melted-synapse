

module Util exposing (..)


-- For when you know that Maybe is always Just
forceUnwrap : Maybe a -> a
forceUnwrap maybe =
  case maybe of
    Nothing ->
      Debug.crash "Impossible"
    Just value ->
      value
