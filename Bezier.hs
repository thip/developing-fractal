module Bezier (smooth) where

import Graphics.Gloss.Data.Picture (Path, Point)
import qualified Graphics.Gloss.Data.Point.Arithmetic as P
import Graphics.Gloss.Data.Vector (mulSV)

-- | Start, handle and end of a quadratic Bezier curve.
type Section = (Point, Point, Point)

-- | Rounds off each corner of a path, keeping its two end points.
smooth :: Int -> Path -> Path
smooth n [a, b] = a : interpolateLine a b n ++ [b]
smooth n path@(a : b : _) =
  a : half a b : concatMap corner (zip3 path (drop 1 path) (drop 2 path)) ++ [half secondLast end, end]
  where
    corner (p, q, r) = interpolateSection (half p q, q, half q r) (2 * n - 1)
    secondLast = last (init path)
    end = last path
smooth _ path = path

half :: Point -> Point -> Point
half a b = pointAlongLine a b 0.5

interpolateLine :: Point -> Point -> Int -> [Point]
interpolateLine a b n = [pointAlongLine a b (fromIntegral x / fromIntegral n + 1) | x <- [1 .. n]]

interpolateSection :: Section -> Int -> [Point]
interpolateSection section n = [pointAlongSection section (fromIntegral x / (fromIntegral n + 1)) | x <- [1 .. n]]

pointAlongSection :: Section -> Float -> Point
pointAlongSection (start, handle, end) d = pointAlongLine s e d
  where
    s = pointAlongLine start handle d
    e = pointAlongLine handle end d

pointAlongLine :: Point -> Point -> Float -> Point
pointAlongLine a b d = a P.+ mulSV d (b P.- a)
