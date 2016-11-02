module Main(main) where

import Graphics.Gloss

data Rule = Rule Char String deriving (Show)
data Axiom = Axiom String deriving (Show)
data LSystem = LSystem Axiom [Rule] deriving (Show)

dragon = LSystem (Axiom "f") [Rule 'f' "f-h", Rule 'h' "f+h"]
segLength = 2

main :: IO()
main = display (InWindow "fractal" (200, 200) (10, 10)) (white) (line ([(0,0)] ++ (makePath (Main.iterate 15 dragon) (0,0) 0 )))


makePath :: String->Vector->Float->Path
makePath [] curPos theta = []
makePath (c:cs) curPos theta
        | c == '+' = makePath cs curPos (theta + (pi/2))
        | c == '-' = makePath cs curPos (theta - (pi/2))
        | otherwise = [curPos + (segLength * (sin theta), segLength * (cos theta))] ++ (makePath cs (curPos + (segLength * (sin theta), segLength * (cos theta))) theta)
    

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
