module Main(main) where

import Graphics.Gloss
import Bezier

data Rule = Rule Char String deriving (Show)
data Axiom = Axiom String deriving (Show)
data Angle = Angle Float deriving (Show)
data LSystem = LSystem Angle Axiom [Rule] deriving (Show)

segLength = 10
iterations = 7

dragon = LSystem (Angle (pi/2)) (Axiom "f") [Rule 'f' "f-h", Rule 'h' "f+h"]
terDragon = LSystem (Angle (2*pi/3)) (Axiom "f") [Rule 'f' "f+f-f"]

main :: IO()
main = display (InWindow "fractal" (300, 300) (100, 100)) (black) (pictures [Color red $  drawSmoothFractal terDragon (Angle 0) segLength iterations, Color blue $  drawSmoothFractal terDragon (Angle (pi/6)) (segLength*1.7320508) $ iterations-1, Color green $  drawSmoothFractal terDragon (Angle (2*pi/6)) (segLength*1.7320508*1.7320508) $ iterations-2])


drawSmoothFractal :: LSystem->Angle->Float->Int->Picture
drawSmoothFractal (LSystem (Angle theta) (Axiom a) rules) (Angle offset) segmentLength  n = line $ smooth ([(0,0)] ++ makePath (iterate' n a rules) (theta) (0,0) offset segmentLength) 16 


drawFractal :: LSystem->Int->Picture
drawFractal (LSystem (Angle theta) (Axiom a) rules) n = line ([(0,0)] ++ makePath (iterate' n a rules) theta (0,0) 0 segLength)

makePath :: String->Float->Vector->Float->Float->Path
makePath [] theta curPos initialTheta l= []
makePath (c:cs) theta curPos initialTheta l
        | c == '+' = makePath cs theta curPos (initialTheta + theta) l
        | c == '-' = makePath cs theta curPos (initialTheta - theta) l
        | otherwise = [newPos] ++ (makePath cs theta newPos initialTheta l)
                where newPos =  curPos + (l * (sin initialTheta), l * (cos initialTheta))

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
