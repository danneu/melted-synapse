

module Util.List exposing (..)


dropRight : Int -> List a -> List a
dropRight n list =
  List.take ((List.length list) - n) list
