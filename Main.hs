module Main(main) where

import Graphics.Gloss

main :: IO()

main = display (InWindow "fractal" (200, 200) (10, 10)) (white) (circle 80)

data Rule = Rule Char String deriving (Show)
data Axiom = Axiom String deriving (Show)
data LSystem = LSystem Axiom [Rule] deriving (Show)

iterate :: Int->LSystem->String
iterate n (LSystem (Axiom a) rules) = iterate' n a rules

iterate' :: Int->String->[Rule]->String
iterate' 0 string rules = string
iterate' n string rules = iterate' (n-1) (replace string rules) rules  

replace :: String->[Rule]->String
replace (c:[]) rules = replace' c rules
replace (c:cs) rules = replace' c rules ++ replace cs rules

replace' :: Char->[Rule]->String
replace' c ((Rule c' s):[]) 
        | c == c'   = s
        | otherwise = [c]
replace' c ((Rule c' s):rs)
        | c == c'   = s
        | otherwise = replace' c rs 
