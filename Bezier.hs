module Bezier (smooth) where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Data.List

type Section = (Point, Point, Point)

smooth :: Integer -> [Point] -> [Point]
smooth n (a:b:[]) = [a] ++ [b]
smooth n (a:b:c:[]) = interpolateSection (a, c, b) n
smooth n (a:b:c:d:[]) = (interpolateSection (a, (half b c), b) n ) ++ (interpolateSection ((half b c), d, c) n )
smooth n (a:b:c:ps) = (smoothEnd n a b c) ++  (smoothMiddle n middle) ++ (smoothOtherEnd n end)
    where middle = [b] ++ [c] ++ reverse (  drop 1 ( reverse ps))
          end  = reverse ( take 3 ( reverse ps))

smoothEnd :: Integer -> Point -> Point -> Point -> [Point]
smoothEnd n a b c = interpolateSection (a, (half b c), b) n

smoothOtherEnd :: Integer -> [Point] -> [Point]
smoothOtherEnd n (a:b:c:[]) = interpolateSection ((half a b), c, b) n
smoothOtherEnd n ps = ps

smoothMiddle :: Integer -> [Point] -> [Point]
smoothMiddle n [] = []
smoothMiddle n (a:[]) = [a]
smoothMiddle n (a:b:[]) = interpolateLine a b n
smoothMiddle n (a:b:c:[]) = interpolateSection ((half a b), (half b c), b) n
smoothMiddle n (a:b:c:ps) = (smoothMiddle n ([a]++[b]++[c]))  ++ (smoothMiddle n ([b] ++ [c] ++ ps)) 

half :: Point -> Point -> Point
half a b = a + (mulSV 0.5 (b-a)) 

interpolateLine :: Point -> Point -> Integer -> [Point]
interpolateLine a b n = [ pointAlongLine a b (x/(fromIntegral n)) | x <- [ fromIntegral x' | x' <- [0..n] ] ]

interpolateSection :: Section -> Integer -> [Point]
interpolateSection section n = [ point section (x/(fromIntegral n)) | x <- [ fromIntegral x' | x' <- [0..n] ] ]

point :: Section -> Float -> Point
point (start, end, handle) n = pointAlongLine s e n
        where s = pointAlongLine start handle n
              e = pointAlongLine handle end n 

pointAlongLine :: Point -> Point -> Float -> Point
pointAlongLine a b d = a + (mulSV  d  (b-a))
