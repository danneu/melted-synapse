
module Vector exposing (..)


type alias Vector = (Float, Float)


fromPoints : Vector -> Vector -> Vector
fromPoints (x1, y1) (x2, y2) =
  ( x2 - x1
  , y2 - y1
  )


normalize : Vector -> Vector
normalize (x, y) =
  let
    len = x * x + y * y
  in
    if len > 0 then
      let
        scale = 1 / (sqrt len)
      in
        (x * scale, y * scale)
    else
      (x, y)


dist : Vector -> Vector -> Float
dist (x1, y1) (x2, y2) =
  let
    x' = x2 - x1
    y' = y2 - y1
  in
    sqrt (x' * x' + y' * y')


scale : Float -> Vector -> Vector
scale scalar (x, y) =
  (x * scalar, y * scalar)


flipY : Vector -> Vector
flipY (x, y) =
  (x, -y)


-- radians
angleTo : Vector -> Vector -> Float
angleTo (x1, y1) (x2, y2) =
  atan2 (y2 - y1) (x2 - x1)


toString : Vector -> String
toString (x, y) =
  "(" ++ (Basics.toString x) ++ ", " ++ (Basics.toString y) ++ ")"
