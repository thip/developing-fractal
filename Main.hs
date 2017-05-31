module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Bezier

data Rule = Rule Char String deriving (Show)
data Axiom = Axiom String deriving (Show)
data Angle = Angle Float deriving (Show)
data LSystem = LSystem Angle Axiom [Rule] deriving (Show)

segLength = 10
iterations = 2

chosenSystem = terDragon
twist = (angleAsFloat . getTwistAngle) chosenSystem
    where angleAsFloat (Angle a) = a

sf = getScaleFactor chosenSystem

dragon = LSystem (Angle (pi/2)) (Axiom "f") [Rule 'f' "f-h", Rule 'h' "f+h"]
terDragon = LSystem (Angle (2*pi/3)) (Axiom "f") [Rule 'f' "f+f-f"]

main :: IO()
main = do 
    print (getScaleFactor chosenSystem)
    display (InWindow "fractal" (300, 300) (100, 100)) (white) (Pictures (scaleAndRotateLayers (drawIterations 0 8 chosenSystem)))

scaleAndRotateLayers :: [Picture] -> [Picture]
scaleAndRotateLayers layers = scaleAndRotateLayers' (reverse layers)  0 

scaleAndRotateLayers' :: [Picture]->Int->[Picture]
scaleAndRotateLayers' [] layerNumber = []
scaleAndRotateLayers' (layer:layers) layerNumber = [(scaleAndRotate (sf**n) (n*a) layer)] ++ (scaleAndRotateLayers' layers (layerNumber+1))
    where n = fromIntegral layerNumber  
          a = twist

scaleAndRotate :: Float->Float->Picture->Picture
scaleAndRotate scaleFac angle picture = scale scaleFac scaleFac (rotate' angle picture)

drawIterations :: Int->Int->LSystem->[Picture]
drawIterations start stop  system = map (drawSmoothFractal system) [start..stop] 

rotate' :: Float->Picture->Picture
rotate' angle picture = rotate (angle * (180/pi)) picture

drawSmoothFractal :: LSystem->Int->Picture
drawSmoothFractal system n = Color (greyN (0.01+ ( fromIntegral n )/10)) $  line $ smooth 32 ([(0,0)] ++ makePath system n) 


drawFractal :: LSystem->Int->Picture
drawFractal system n = line ([(0,0)] ++ makePath system n)

makePath :: LSystem->Int->Path
makePath (LSystem (Angle theta) (Axiom a) rules) n  = makePath' (iterate' n a rules) theta (0,0) 0

makePath' :: String->Float->Vector->Float->Path
makePath' [] theta curPos initialTheta = []
makePath' (c:cs) theta curPos initialTheta
        | c == '+' = makePath' cs theta curPos (initialTheta + theta)
        | c == '-' = makePath' cs theta curPos (initialTheta - theta)
        | otherwise = [newPos] ++ (makePath' cs theta newPos initialTheta)
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

getScaleFactor :: LSystem->Float
getScaleFactor system = (magV delta1)/(magV delta0)
    where delta0 = (last path0)
          delta1 = (last path1)
          path0 = makePath system 0
          path1 = makePath system 1

getTwistAngle :: LSystem->Angle
getTwistAngle system  = Angle ((argV sum1) - (argV sum0))
    where sum0 = (last path0) - (head path0)
          sum1 = (last path1) - (head path1)
          path0 = makePath system 0
          path1 = makePath system 1
