-- | Stacks successive iterations of an L-system so that they can be joined into
-- a surface.
--
-- Each iteration of a dragon curve is a larger, rotated copy of the shape of the
-- one before. To stack them, every iteration is smoothed to a similar level of
-- detail, given the same number of points, and scaled and rotated so that they
-- all start and end in the same place.
module Layers
  ( Layers
  , LayerSettings (..)
  , validateSettings
  , buildLayers
  , layerPaths
  , scaleFactor
  , twistAngle
  ) where

import Curve (resample, smooth)
import LSystem (LSystem, makePath)
import Vector (Path, Vector, direction, magnitude, rotateVector, scaleVector)

data LayerSettings = LayerSettings
  { startIteration :: Int
  , endIteration :: Int
  , smoothness :: Int
  }
  deriving (Show)

-- | Aligned paths, finest iteration first, all with the same number of points.
-- The constructor isn't exported, so 'buildLayers' is the only way to make one
-- and those properties always hold.
newtype Layers = Layers [Path]

layerPaths :: Layers -> [Path]
layerPaths (Layers paths) = paths

validateSettings :: LayerSettings -> Either String LayerSettings
validateSettings settings
  | startIteration settings < 0 = Left "start must be at least 0"
  | endIteration settings < startIteration settings = Left "end must be at least start"
  | smoothness settings < 1 = Left "smoothness must be at least 1"
  | otherwise = Right settings

buildLayers :: LSystem -> LayerSettings -> Layers
buildLayers system settings = Layers (alignCurves system (matchPointCounts smoothedPaths))
  where
    LayerSettings {startIteration = start, endIteration = end} = settings
    smoothedPaths = map smoothedPath [start .. end]
    -- Coarser iterations get more passes, so that all layers are similarly
    -- detailed before their point counts are matched.
    smoothedPath n = iterate (smooth (smoothness settings)) (makePath system n) !! (end + 1 - n)

-- | Gives every path the point count of the longest one.
matchPointCounts :: [Path] -> [Path]
matchPointCounts paths = map (resample (maximum (map length paths))) paths

-- | Scales and rotates each curve to line up with the others. Takes the curves
-- coarsest first and returns them finest first: the finest needs the least
-- scaling, as each iteration is already larger than the one before.
alignCurves :: LSystem -> [Path] -> [Path]
alignCurves system = zipWith align [1 :: Int ..] . reverse
  where
    align n = scaleAndRotate (scaleFactor system ** fromIntegral n) (angle * fromIntegral n)
    angle = negate (twistAngle system) -- negative because the curves are reversed

scaleAndRotate :: Float -> Float -> Path -> Path
scaleAndRotate factor angle = map (scaleVector factor . rotateVector angle)

-- | How much longer the curve gets from end to end with each iteration.
scaleFactor :: LSystem -> Float
scaleFactor system = magnitude (endPoint system 1) / magnitude (endPoint system 0)

-- | How far the curve's end-to-end direction turns with each iteration.
twistAngle :: LSystem -> Float
twistAngle system = direction (endPoint system 0) - direction (endPoint system 1)

-- | Where the curve finishes. Paths always start at the origin, so this is also
-- its overall displacement.
endPoint :: LSystem -> Int -> Vector
endPoint system = last . makePath system
