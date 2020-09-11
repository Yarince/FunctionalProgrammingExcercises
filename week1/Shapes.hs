{-
  Group 82

  Tobias Cunnen - S1047865
  Yarince Martis - S1061681
-}

module Shapes where

data Shape
  = Circle Double -- radius
  | Square Double -- length
  | Rectangle Double Double -- length and width
  deriving (Show)

showShape :: Shape -> String
showShape (Circle r) = "circle of radius " ++ show r
showShape (Square l) = "square of length " ++ show l
showShape (Rectangle l w) =
  "rectangle of length " ++ show l
    ++ " and width "
    ++ show w

area :: Shape -> Double
area (Circle r) = r ^ 2 * pi
area (Square l) = l * l
area (Rectangle l w) = l * w

perimeter :: Shape -> Double
perimeter (Circle r) = 2 * pi * r
perimeter (Square l) = 4 * l
perimeter (Rectangle l w) = 2 * (l + w)

{-
  So the for calculating the circle we chose to draw it in the right upper plane
  of a x and y graph instead of the center. Because that would always return 0,0.
  We asked the TA but he was also not sure so we just chose this.
-}
center :: Shape -> (Double, Double) -- x- and y-coordinates
center (Circle r) = (r, r)
center (Square l) = (l / 2, l / 2)
center (Rectangle l w) = (w / 2, l / 2)

boundingBox :: Shape -> (Double, Double) -- width and height
boundingBox (Circle r) = let d = r * 2 in (d, d) -- Not optimal I think, just testing in function constants
boundingBox (Square l) = (l, l)
boundingBox (Rectangle l w) = (w, l)
