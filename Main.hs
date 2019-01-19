module Main(main) where

import Text.Printf
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Data.List
import Bezier

data Rule = Rule Char String deriving (Show)
newtype Axiom = Axiom String deriving (Show)
newtype  Angle = Angle Float deriving (Show)
data LSystem = LSystem Angle Axiom [Rule] deriving (Show)

chosenSystem = terDragon
startIterations = 1
endIterations = 5
smoothness = 2

dragon = LSystem (Angle (pi/2)) (Axiom "f") [Rule 'f' "f-h", Rule 'h' "f+h"]
terDragon = LSystem (Angle (2*pi/3)) (Axiom "f") [Rule 'f' "f+f-f"]

main :: IO()
main = do

--    mapM_ (print) lists
--    where lists = [map (\a->(a,a)) [0..n] | n <- [1..10]]
--      mapM_ (print) (map length alignedPaths)
--      display  (InWindow "fractal" (300, 300) (100, 100)) (white) (Pictures layers)
    putStrLn (makeObj ( map (scale' 10) alignedPaths) ++ faces alignedPaths)
        where layers = map line alignedPaths ++ map markPoints alignedPaths
              alignedPaths = scaleAndRotateCurves chosenSystem paths'
              paths' = [ selfComposeN (endIterations-n) (smooth 2) (makePath chosenSystem n) | n <- [startIterations..endIterations] ]   

showFloat :: Float->String
showFloat = printf "%.10f"

makeObj :: [[Vector]] -> String
makeObj curves = (concat.concat) $ map 
                                    (\(order, curve) 
                                        -> (map (makeVectorString . addZ (10*2^order)) curve )) 
                                    (zip [0..] curves)

makeVectorString ::  (Float,Float,Float)->String
makeVectorString (x,y,z) = printf "v %0.10f %0.10f %0.10f\n" x y z

addZ :: Float -> (Float, Float) -> (Float, Float,Float)
addZ z (x,y) = (x, y, z)

faces :: [[Vector]]->String
faces (c:curves) = concat [ printf "f %d %d %d\nf %d %d %d\n" a b c b d c  | (a,b,c,d) <- squares] 
    where squares = [(p, p+1, p+width, p+width+1) | p <- topCorners]
          topCorners = [ x+y  | x  <- [0..(width-2)], y <- layerStarts ] 
          layerStarts = [ 1+n*width | n  <- [0..(height-2)]]
          height = 1 + length curves 
          width = length c

(^*) f n = selfComposeN n f 

selfComposeN :: Integer -> (a->a) -> a -> a
selfComposeN 0 function arg = function arg
selfComposeN n function arg = function (selfComposeN (n-1) function arg)

markPoints :: [Vector] -> Picture
markPoints points = Pictures (map drawCircle points)
    where drawCircle (x,y) = translate x y (Circle 0.1)

scaleAndRotateCurves :: LSystem->[[Vector]]->[[Vector]]
scaleAndRotateCurves system curves = map (\(n, curve)->scaleAndRotate (scaleFactor**fromIntegral n) (angle*fromIntegral n) curve) (addIndicies (reverse curves)) 
    where angle = -( ( \(Angle a) -> a ) . getTwistAngle) system -- -ve because we've reversed the order of the curves
          scaleFactor = getScaleFactor system

addIndicies :: [a]->[(Integer, a)]
addIndicies = zip [1..]

scaleAndRotate :: Float->Float->[Vector]->[Vector]
scaleAndRotate scaleFactor angle = scale' scaleFactor . rotate' angle

scale' :: Float->[Vector]->[Vector]
scale' scaleFactor= map (mulSV scaleFactor)

rotate' :: Float->[Vector]->[Vector]
rotate' = map .  rotateV


makePath :: LSystem->Integer->Path
makePath (LSystem (Angle theta) (Axiom a) rules) n  = (0,0) :  makePath' (iterate' n a rules) theta (0,0) 0

makePath' :: String->Float->Vector->Float->Path
makePath' [] theta curPos initialTheta = []
makePath' (c:cs) theta curPos initialTheta
        | c == '+' = makePath' cs theta curPos (initialTheta + theta)
        | c == '-' = makePath' cs theta curPos (initialTheta - theta)
        | otherwise = newPos : makePath' cs theta newPos initialTheta
                where newPos =  curPos + (sin initialTheta, cos initialTheta)

iterate' :: Integer->String->[Rule]->String
iterate' 0 string rules = string
iterate' n string rules = iterate' (n-1) (replace string rules) rules  

replace :: String->[Rule]->String
replace [c] rules = replace' c rules
replace (c:cs) rules = replace' c rules ++ replace cs rules

replace' :: Char->[Rule]->String
replace' c [Rule c' s]
        | c == c'   = s
        | otherwise = [c]
replace' c (Rule c' s:rs)
        | c == c'   = s
        | otherwise = replace' c rs 

getScaleFactor :: LSystem->Float
getScaleFactor system = magV delta1/magV delta0
    where delta0 = last path0
          delta1 = last path1
          path0 = makePath system 0
          path1 = makePath system 1

getTwistAngle :: LSystem->Angle
getTwistAngle system  = Angle (argV sum0 - argV sum1)
    where sum0 = last path0 - head path0
          sum1 = last path1 - head path1
          path0 = makePath system 0
          path1 = makePath system 1
