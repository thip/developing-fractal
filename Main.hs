module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Bezier

data Rule = Rule Char String deriving (Show)
data Axiom = Axiom String deriving (Show)
data Angle = Angle Float deriving (Show)
data LSystem = LSystem Angle Axiom [Rule] deriving (Show)

chosenSystem = terDragon
startIterations = 0
endIterations =3


dragon = LSystem (Angle (pi/2)) (Axiom "f") [Rule 'f' "f-h", Rule 'h' "f+h"]
terDragon = LSystem (Angle (2*pi/3)) (Axiom "f") [Rule 'f' "f+f-f"]

main :: IO()
main = do
    mapM_ (print) (map length smoothPaths)
    display  (InWindow "fractal" (300, 300) (100, 100)) (white) (Pictures layers)
        where layers = (map line smoothPaths) ++ map markPoints smoothPaths
              smoothPaths = (map (\(n, path) -> (smooth (2^n) path)) (addIndicies (alignedPaths)))
              alignedPaths = scaleAndRotateCurves chosenSystem paths'
              paths = map ((scale' 10).(makePath chosenSystem)) [startIterations..endIterations]
	      paths' = [((smooth 3) ^* (endIterations-n)) ((makePath chosenSystem n)) | n <- [startIterations..endIterations] ]   

(^*) f n = selfComposeN n f 

selfComposeN :: Integer -> (a->a) -> a -> a
selfComposeN 0 function arg = function arg
selfComposeN n function arg = function (selfComposeN (n-1) (function) (arg))

markPoints :: [Vector] -> Picture
markPoints points = Pictures (map drawCircle points)
    where drawCircle (x,y) = translate x y (Circle 0.1)

scaleAndRotateCurves :: LSystem->[[Vector]]->[[Vector]]
scaleAndRotateCurves system curves = map (\(n, curve)->scaleAndRotate (scaleFactor**(fromIntegral n)) (angle*(fromIntegral n)) curve) (addIndicies (reverse curves)) 
    where angle = -( ( \(Angle a) -> a ) . getTwistAngle) system -- -ve because we've reversed the order of the curves
          scaleFactor = getScaleFactor system

addIndicies :: [a]->[(Integer, a)]
addIndicies xs = zip [0..] xs

scaleAndRotate :: Float->Float->[Vector]->[Vector]
scaleAndRotate scaleFactor angle points = ((scale' scaleFactor) . (rotate' angle)) points

scale' :: Float->[Vector]->[Vector]
scale' scaleFactor points = map (mulSV scaleFactor) points

rotate' :: Float->[Vector]->[Vector]
rotate' angle points = map (rotateV angle) points


makePath :: LSystem->Integer->Path
makePath (LSystem (Angle theta) (Axiom a) rules) n  = [(0,0)] ++ makePath' (iterate' n a rules) theta (0,0) 0

makePath' :: String->Float->Vector->Float->Path
makePath' [] theta curPos initialTheta = []
makePath' (c:cs) theta curPos initialTheta
        | c == '+' = makePath' cs theta curPos (initialTheta + theta)
        | c == '-' = makePath' cs theta curPos (initialTheta - theta)
        | otherwise = [newPos] ++ (makePath' cs theta newPos initialTheta)
                where newPos =  curPos + (sin initialTheta, cos initialTheta)

iterate' :: Integer->String->[Rule]->String
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
getTwistAngle system  = Angle ((argV sum0) - (argV sum1))
    where sum0 = (last path0) - (head path0)
          sum1 = (last path1) - (head path1)
          path0 = makePath system 0
          path1 = makePath system 1
