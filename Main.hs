module Main (main, dragon, terDragon) where

import Data.Maybe (fromMaybe)
import Graphics.Gloss.Data.Picture (Path)
import qualified Graphics.Gloss.Data.Point.Arithmetic as P
import Graphics.Gloss.Data.Vector (Vector, argV, magV, mulSV, rotateV)
import Text.Printf (printf)

import Bezier (smooth)

type Rule = (Char, String)

newtype Axiom = Axiom String deriving (Show)
newtype Angle = Angle Float deriving (Show)
data LSystem = LSystem Angle Axiom [Rule] deriving (Show)

dragon, terDragon :: LSystem
dragon = LSystem (Angle (pi / 2)) (Axiom "f") [('f', "f-h"), ('h', "f+h")]
terDragon = LSystem (Angle (2 * pi / 3)) (Axiom "f") [('f', "f+f-f")]

chosenSystem :: LSystem
chosenSystem = terDragon

startIterations, endIterations, smoothness :: Int
startIterations = 1
endIterations = 5
smoothness = 2

meshScale, baseLayerHeight :: Float
meshScale = 10
baseLayerHeight = 10

main :: IO ()
main = putStrLn (objVertices (map (scalePath meshScale) alignedPaths) ++ objFaces alignedPaths)
  where
    alignedPaths = alignCurves chosenSystem smoothedPaths
    smoothedPaths = map smoothedPath [startIterations .. endIterations]
    -- Each pass brings a layer up to the point count of the next iteration, so
    -- every layer ends up matching iteration (endIterations + 1).
    smoothedPath n = iterate (smooth smoothness) (makePath chosenSystem n) !! (endIterations + 1 - n)

-- OBJ output

objVertices :: [[Vector]] -> String
objVertices = concat . zipWith layer [0 :: Int ..]
  where
    layer order = concatMap (vertexLine . addZ (baseLayerHeight * 2 ^ order))

vertexLine :: (Float, Float, Float) -> String
vertexLine (x, y, z) = printf "v %0.10f %0.10f %0.10f\n" x y z

addZ :: Float -> Vector -> (Float, Float, Float)
addZ z (x, y) = (x, y, z)

-- | Joins neighbouring layers into triangles. Assumes every layer has the same
-- number of points. OBJ vertex indices start at 1.
objFaces :: [[Vector]] -> String
objFaces [] = ""
objFaces curves@(curve : _) = concatMap quad topCorners
  where
    quad p = printf "f %d %d %d\nf %d %d %d\n" a b c b d c
      where
        (a, b, c, d) = (p, p + 1, p + width, p + width + 1)
    topCorners = [x + y | x <- [0 .. width - 2], y <- layerStarts]
    layerStarts = [1 + n * width | n <- [0 .. height - 2]]
    height = length curves
    width = length curve

-- Layer alignment

alignCurves :: LSystem -> [[Vector]] -> [[Vector]]
alignCurves system = zipWith align [1 :: Int ..] . reverse
  where
    align n = scaleAndRotate (scaleFactor system ** fromIntegral n) (angle * fromIntegral n)
    angle = negate (twistAngle system) -- negative because the curves are reversed

scaleAndRotate :: Float -> Float -> [Vector] -> [Vector]
scaleAndRotate factor angle = scalePath factor . rotatePath angle

scalePath :: Float -> [Vector] -> [Vector]
scalePath = map . mulSV

rotatePath :: Float -> [Vector] -> [Vector]
rotatePath = map . rotateV

scaleFactor :: LSystem -> Float
scaleFactor system = magV (endPoint system 1) / magV (endPoint system 0)

twistAngle :: LSystem -> Float
twistAngle system = argV (endPoint system 0) - argV (endPoint system 1)

-- | Where the curve finishes. Paths always start at the origin, so this is also
-- its overall displacement.
endPoint :: LSystem -> Int -> Vector
endPoint system = last . makePath system

-- L-system

makePath :: LSystem -> Int -> Path
makePath system@(LSystem (Angle theta) _ _) n = scanl (P.+) (0, 0) steps
  where
    commands = derive system n
    headings = scanl turn 0 commands
    steps = [(sin heading, cos heading) | (c, heading) <- zip commands headings, not (isTurn c)]

    turn heading '+' = heading + theta
    turn heading '-' = heading - theta
    turn heading _ = heading

isTurn :: Char -> Bool
isTurn c = c == '+' || c == '-'

derive :: LSystem -> Int -> String
derive (LSystem _ (Axiom start) rules) n = iterate (expand rules) start !! n

expand :: [Rule] -> String -> String
expand rules = concatMap (\c -> fromMaybe [c] (lookup c rules))
