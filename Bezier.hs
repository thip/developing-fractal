module Bezier (smooth) where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Data.List

type Section = (Point, Point, Point)

smooth :: Integer -> [Point] -> [Point]
smooth n [a,b] = [a] ++ interpolateLine a b n ++ [b]
smooth n (a:b:ps) = [a] ++ [half a b] ++ smooth' n (a:b:ps) 

smooth' n [a,b] = [half a b] ++ [b]
smooth' n (a:b:c:ps) = interpolateSection (half a b, b, half b c) (2*n-1) ++ smooth' n (b:c:ps)

half :: Point -> Point -> Point
half a b = a + mulSV 0.5 (b-a) 

interpolateLine :: Point -> Point -> Integer -> [Point]
interpolateLine a b n = [ pointAlongLine a b (x/fromIntegral n+1) | x <- [ fromIntegral x' | x' <- [1..n] ] ]

interpolateSection :: Section -> Integer -> [Point]
interpolateSection section n = [ pointAlongSection section (x/(fromIntegral n+1)) | x <- [ fromIntegral x' | x' <- [1..n] ] ]

pointAlongSection :: Section -> Float -> Point
pointAlongSection (start, handle, end) n = pointAlongLine s e n
        where s = pointAlongLine start handle n
              e = pointAlongLine handle end n 

pointAlongLine :: Point -> Point -> Float -> Point
pointAlongLine a b d = a + mulSV  d  (b-a)

