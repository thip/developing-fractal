-- | L-systems, and turning them into paths.
module LSystem
  ( LSystem (..)
  , Angle (..)
  , Axiom (..)
  , Rule
  , dragon
  , terDragon
  , systems
  , derive
  , expand
  , makePath
  ) where

import Data.Maybe (fromMaybe)

import Vector (Path, (^+^))

-- | A symbol and the string that replaces it on each iteration.
type Rule = (Char, String)

newtype Axiom = Axiom String deriving (Show)

-- | The angle in radians that @+@ and @-@ turn by.
newtype Angle = Angle Float deriving (Show)

data LSystem = LSystem Angle Axiom [Rule] deriving (Show)

-- | @f@ and @h@ both draw a step. They differ only in how they expand.
dragon :: LSystem
dragon = LSystem (Angle (pi / 2)) (Axiom "f") [('f', "f-h"), ('h', "f+h")]

terDragon :: LSystem
terDragon = LSystem (Angle (2 * pi / 3)) (Axiom "f") [('f', "f+f-f")]

-- | The available systems, by the name used on the command line.
systems :: [(String, LSystem)]
systems = [("terdragon", terDragon), ("dragon", dragon)]

-- | Expands the axiom a given number of times.
derive :: LSystem -> Int -> String
derive (LSystem _ (Axiom start) rules) n = iterate (expand rules) start !! n

expand :: [Rule] -> String -> String
expand rules = concatMap (\c -> fromMaybe [c] (lookup c rules))

-- | Walks an iteration of the system as a turtle starting at the origin and
-- heading along the y axis. @+@ and @-@ turn, and any other symbol draws a step
-- of length 1.
makePath :: LSystem -> Int -> Path
makePath system@(LSystem (Angle theta) _ _) n = scanl (^+^) (0, 0) steps
  where
    commands = derive system n
    headings = scanl turn 0 commands
    steps = [(sin heading, cos heading) | (c, heading) <- zip commands headings, not (isTurn c)]

    turn heading '+' = heading + theta
    turn heading '-' = heading - theta
    turn heading _ = heading

isTurn :: Char -> Bool
isTurn c = c == '+' || c == '-'
