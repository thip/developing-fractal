-- | Minimal 2D vector arithmetic.
module Vector
  ( Vector
  , Path
  , (^+^)
  , (^-^)
  , scaleVector
  , rotateVector
  , magnitude
  , direction
  ) where

type Vector = (Float, Float)

type Path = [Vector]

infixl 6 ^+^, ^-^

(^+^) :: Vector -> Vector -> Vector
(x1, y1) ^+^ (x2, y2) = (x1 + x2, y1 + y2)

(^-^) :: Vector -> Vector -> Vector
(x1, y1) ^-^ (x2, y2) = (x1 - x2, y1 - y2)

scaleVector :: Float -> Vector -> Vector
scaleVector s (x, y) = (s * x, s * y)

-- | Rotates anticlockwise by an angle in radians.
rotateVector :: Float -> Vector -> Vector
rotateVector r (x, y) = (x * cos r - y * sin r, x * sin r + y * cos r)

magnitude :: Vector -> Float
magnitude (x, y) = sqrt (x * x + y * y)

-- | The angle of a vector in radians, in the range [0, 2pi).
direction :: Vector -> Float
direction (x, y) = normalise (atan2 y x)
  where
    normalise a = a - 2 * pi * fromIntegral (floor (a / (2 * pi)) :: Int)
