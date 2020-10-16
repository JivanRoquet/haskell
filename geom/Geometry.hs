module Geometry where

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point
             deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r * 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * abs (y2 - y1)

circle1 = Circle (Point 2.3 5.6) 4.0
rect1 = Rectangle (Point 2.1 4.5) (Point 5.6 8.2)