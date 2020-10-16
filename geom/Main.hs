module Main where

import Geometry

main :: IO ()
main = do
    print $ centroid circ1 <=> centroid rect1
    print $ centroid circ2 <=> centroid rect2
    print $ perimeter circ1
    print $ perimeter rect1