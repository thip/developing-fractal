module Bezier (smooth, resample) where

import Data.Array (listArray, (!))
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

-- | Evenly spaces @count@ points along a path by index, keeping its two end
-- points. A path that already has @count@ points is returned unchanged.
resample :: Int -> Path -> Path
resample count path
  | size == count || size < 2 || count < 2 = path
  | otherwise = map pointAt [0 .. count - 1]
  where
    size = length path
    points = listArray (0, size - 1) path
    pointAt i
      | offset == 0 = points ! segment
      | otherwise = pointAlongLine (points ! segment) (points ! (segment + 1)) fraction
      where
        (segment, offset) = (i * (size - 1)) `divMod` (count - 1)
        fraction = fromIntegral offset / fromIntegral (count - 1)

half :: Point -> Point -> Point
half a b = pointAlongLine a b 0.5

interpolateLine :: Point -> Point -> Int -> [Point]
interpolateLine a b n = [pointAlongLine a b (fromIntegral x / (fromIntegral n + 1)) | x <- [1 .. n]]

interpolateSection :: Section -> Int -> [Point]
interpolateSection section n = [pointAlongSection section (fromIntegral x / (fromIntegral n + 1)) | x <- [1 .. n]]

pointAlongSection :: Section -> Float -> Point
pointAlongSection (start, handle, end) d = pointAlongLine s e d
  where
    s = pointAlongLine start handle d
    e = pointAlongLine handle end d

pointAlongLine :: Point -> Point -> Float -> Point
pointAlongLine a b d = a P.+ mulSV d (b P.- a)
