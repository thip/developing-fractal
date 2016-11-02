module Bezier (smooth) where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Data.List

type Section = (Point, Point, Point)

smooth :: [Point] -> Int -> [Point]
smooth (a:b:c:[]) n = interpolateSection ((half a b), (half b c), b) n
smooth (a:b:c:ps) n = (smooth ([a]++[b]++[c]) n)  ++ (smooth ([b] ++ [c] ++ ps) n) 

half :: Point -> Point -> Point
half a b = a + (mulSV 0.5 (b-a)) 

interpolateSection :: Section -> Int -> [Point]
interpolateSection section n = [ point section (x/(fromIntegral n)) | x <- [ fromIntegral x' | x' <- [0..n] ] ]

point :: Section -> Float -> Point
point (start, end, handle) n = pointAlongLine s e n
        where s = pointAlongLine start handle n
              e = pointAlongLine handle end n 

pointAlongLine :: Point -> Point -> Float -> Point
pointAlongLine a b d = a + (mulSV  d  (b-a))
