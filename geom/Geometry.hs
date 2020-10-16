module Geometry where

data Point = Point {
    x :: Float,
    y :: Float
} deriving (Show)

data Circle = Circle {
    c :: Point,
    r :: Float
} deriving (Show)

data Rectangle = Rectangle {
    p1 :: Point,
    p2 :: Point
} deriving (Show)


class Shape a where
    area :: a -> Float
    centroid :: a -> Point
    perimeter :: a -> Float
    isin :: Point -> a -> Bool

instance Shape Circle where
    area (Circle _ r) = 2 * pi * r
    centroid (Circle p _) = p
    perimeter (Circle _ r) = pi * r^2
    isin p (Circle c r) = p <=> c < r

instance Shape Rectangle where
    area (Rectangle p1 p2) = abs (x p2 - x p1) * abs (y p2 - y p1)
    centroid (Rectangle p1 p2) = Point ((x p1 + x p2) / 2) ((y p1 + y p2) / 2)
    perimeter (Rectangle p1 p2) = abs (x p2 - x p1) + abs (y p2 - y p1)
    isin p (Rectangle p1 p2) = x p1 <= x p && x p <= x p2 && y p1 <= y p && y p <= y p2


-- | distance between two points
(<=>) :: Point -> Point -> Float
(<=>) p1 p2 = sqrt $ (x p2 - x p1)^2 + (y p2 - y p1)^2


circ1  = Circle (Point 2.3 3.6) 4.0
circ2  = Circle (Point 6.3 8.6) 6.2
rect1  = Rectangle (Point 2.1 4.5) (Point 5.6 8.2)
rect2  = Rectangle (Point 1.1 0.5) (Point 3.6 4.2)
point1 = Point 3.0 4.2
point2 = Point 1.8 3.1