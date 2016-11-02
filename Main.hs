module Main(main) where

import Graphics.Gloss

data Rule = Rule Char String deriving (Show)
data Axiom = Axiom String deriving (Show)
data Angle = Angle Float deriving (Show)
data LSystem = LSystem Angle Axiom [Rule] deriving (Show)

segLength = 2
iterations = 10

dragon = LSystem (Angle (pi/2)) (Axiom "f") [Rule 'f' "f-h", Rule 'h' "f+h"]
terDragon = LSystem (Angle (2*pi/3)) (Axiom "f") [Rule 'f' "f+f-f"]

main :: IO()
main = display (InWindow "fractal" (200, 200) (10, 10)) (white) (drawFractal dragon iterations)

drawFractal :: LSystem->Int->Picture
drawFractal (LSystem (Angle theta) (Axiom a) rules) n = line ([(0,0)] ++ makePath (iterate' n a rules) theta (0,0) 0)

makePath :: String->Float->Vector->Float->Path
makePath [] theta curPos initialTheta = []
makePath (c:cs) theta curPos initialTheta
        | c == '+' = makePath cs theta curPos (initialTheta + theta)
        | c == '-' = makePath cs theta curPos (initialTheta - theta)
        | otherwise = [newPos] ++ (makePath cs theta newPos initialTheta)
                where newPos =  curPos + (segLength * (sin initialTheta), segLength * (cos initialTheta))

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
