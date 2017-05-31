module Main(main) where

import Graphics.Gloss
import Bezier

data Rule = Rule Char String deriving (Show)
data Axiom = Axiom String deriving (Show)
data Angle = Angle Float deriving (Show)
data LSystem = LSystem Angle Axiom [Rule] deriving (Show)

segLength = 10
iterations = 2
sf = tan (pi/3)

dragon = LSystem (Angle (pi/2)) (Axiom "f") [Rule 'f' "f-h", Rule 'h' "f+h"]
terDragon = LSystem (Angle (2*pi/3)) (Axiom "f") [Rule 'f' "f+f-f"]

main :: IO()
main = display (InWindow "fractal" (300, 300) (100, 100)) (white) (Pictures (scaleAndRotateLayers (drawIterations 0 8 terDragon)))


scaleAndRotateLayers :: [Picture] -> [Picture]
scaleAndRotateLayers layers = scaleAndRotateLayers' (reverse layers)  0 

scaleAndRotateLayers' :: [Picture]->Int->[Picture]
scaleAndRotateLayers' [] layerNumber = []
scaleAndRotateLayers' (layer:layers) layerNumber = [(scaleAndRotate (sf**n) (n*pi/6) layer)] ++ (scaleAndRotateLayers' layers (layerNumber+1))
    where n = fromIntegral layerNumber  

scaleAndRotate :: Float->Float->Picture->Picture
scaleAndRotate scaleFac angle picture = scale scaleFac scaleFac (rotate' angle picture)

drawIterations :: Int->Int->LSystem->[Picture]
drawIterations start stop  system = map (drawSmoothFractal system) [start..stop] 

rotate' :: Float->Picture->Picture
rotate' angle picture = rotate (angle * (180/pi)) picture

drawSmoothFractal :: LSystem->Int->Picture
drawSmoothFractal (LSystem (Angle theta) (Axiom a) rules) n = Color (greyN ( ( fromIntegral n )/10)) $  line $ smooth 32 ([(0,0)] ++ makePath (iterate' n a rules) theta (0,0) 0) 


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
