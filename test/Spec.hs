{-# OPTIONS_GHC -Wno-x-partial #-}

module Main (main) where

import Data.List (nub, sort)
import Test.Hspec
import Test.QuickCheck

import Curve (resample, smooth)
import Layers
import LSystem
import Obj
import Vector

main :: IO ()
main = hspec $ do
  describe "Vector" $ do
    it "adds, subtracts and scales" $ do
      (1, 2) ^+^ (3, 4) `shouldBe` (4, 6)
      (3, 4) ^-^ (1, 2) `shouldBe` (2, 2)
      scaleVector 2 (1, 2) `shouldBe` (2, 4)

    it "rotates anticlockwise" $
      rotateVector (pi / 2) (1, 0) `shouldSatisfy` closeToPoint (0, 1)

    it "measures length" $
      magnitude (3, 4) `shouldBe` 5

    it "gives directions in the range 0 to 2pi" $ do
      direction (0, 1) `shouldSatisfy` closeTo (pi / 2)
      direction (0, -1) `shouldSatisfy` closeTo (3 * pi / 2)
      direction (-1, -1) `shouldSatisfy` closeTo (5 * pi / 4)

  describe "LSystem" $ do
    it "replaces symbols that have a rule and keeps the rest" $
      expand [('a', "ab")] "a+c" `shouldBe` "ab+c"

    it "handles an empty string and no rules" $ do
      expand [('a', "b")] "" `shouldBe` ""
      expand [] "abc" `shouldBe` "abc"

    it "derives the terdragon's first iterations" $ do
      derive terDragon 0 `shouldBe` "f"
      derive terDragon 1 `shouldBe` "f+f-f"

    it "has one more point than segments" $ do
      length (makePath terDragon 3) `shouldBe` 3 ^ (3 :: Int) + 1
      length (makePath dragon 3) `shouldBe` 2 ^ (3 :: Int) + 1

    it "starts at the origin and heads along the y axis" $
      take 2 (makePath terDragon 0) `shouldBe` [(0, 0), (0, 1)]

  describe "smooth" $ do
    it "keeps the end points" $
      property $ \(PathOfAtLeast2 path) (Positive n) ->
        let smoothed = smooth (min n 5) path
         in (head smoothed, last smoothed) === (head path, last path)

    it "leaves paths too short to smooth alone" $ do
      smooth 2 [] `shouldBe` []
      smooth 2 [(1, 2)] `shouldBe` [(1, 2)]

    it "stays on a two point line, in order" $ do
      let ys = map snd (smooth 2 [(0, 0), (0, 3)])
      length ys `shouldBe` 4
      sort ys `shouldBe` ys
      minimum ys `shouldBe` 0
      maximum ys `shouldBe` 3

  describe "resample" $ do
    it "produces the requested number of points" $
      property $ \(PathOfAtLeast2 path) (Positive extra) ->
        let count = 1 + extra
         in length (resample count path) === count

    it "keeps the end points" $
      property $ \(PathOfAtLeast2 path) (Positive extra) ->
        let resampled = resample (1 + extra) path
         in (head resampled, last resampled) === (head path, last path)

    it "leaves a path that already has the right count unchanged" $
      property $ \(PathOfAtLeast2 path) ->
        resample (length path) path === path

  describe "Layers" $ do
    it "derives scale and twist from the system" $ do
      scaleFactor terDragon `shouldSatisfy` closeTo (sqrt 3)
      scaleFactor dragon `shouldSatisfy` closeTo (sqrt 2)
      twistAngle terDragon `shouldSatisfy` closeTo (pi / 6)
      twistAngle dragon `shouldSatisfy` closeTo (negate pi / 4)

    it "rejects invalid settings" $ do
      isValid (LayerSettings (-1) 5 2) `shouldBe` False
      isValid (LayerSettings 3 2 2) `shouldBe` False
      isValid (LayerSettings 1 5 0) `shouldBe` False
      isValid (LayerSettings 0 0 1) `shouldBe` True

    mapM_ layerTests [(name, system, settings) | (name, system) <- systems, settings <- settingsToTest]

  describe "Obj" $ do
    it "matches the output from before the restructure" $ do
      let render system end = renderObj (meshFromLayers (buildLayers system (LayerSettings 1 end 2))) ++ "\n"
      terGolden <- readFile "test/golden/terdragon-1-2.obj"
      dragonGolden <- readFile "test/golden/dragon-1-3.obj"
      render terDragon 2 `shouldBe` terGolden
      render dragon 3 `shouldBe` dragonGolden

    it "writes faces after vertices" $ do
      let obj = lines (renderObj (Mesh [(1, 2, 3)] [(1, 1, 1)]))
      obj `shouldBe` ["v 1.0000000000 2.0000000000 3.0000000000", "f 1 1 1"]

    mapM_ meshTests [(name, system, settings) | (name, system) <- systems, settings <- settingsToTest]

settingsToTest :: [LayerSettings]
settingsToTest =
  [ LayerSettings 1 4 2
  , LayerSettings 0 3 2
  , LayerSettings 1 3 1
  , LayerSettings 1 3 3
  ]

layerTests :: (String, LSystem, LayerSettings) -> Spec
layerTests (name, system, settings) = describe (name ++ " " ++ show settings) $ do
  let paths = layerPaths (buildLayers system settings)

  it "has one layer per iteration" $
    length paths `shouldBe` endIteration settings - startIteration settings + 1

  it "gives every layer the same number of points" $
    length (nub (map length paths)) `shouldBe` 1

  it "lines up the end of every layer" $ do
    let ends = map last paths
    mapM_ (`shouldSatisfy` closeToPoint (head ends)) ends

meshTests :: (String, LSystem, LayerSettings) -> Spec
meshTests (name, system, settings) = describe (name ++ " " ++ show settings) $ do
  let paths = layerPaths (buildLayers system settings)
      Mesh vs ts = meshFromLayers (buildLayers system settings)
      height = length paths
      width = length (head paths)
      row i = (i - 1) `div` width
      column i = (i - 1) `mod` width

  it "has a vertex for every point and two triangles per quad" $ do
    length vs `shouldBe` width * height
    length ts `shouldBe` 2 * (width - 1) * (height - 1)

  it "only joins neighbouring points on neighbouring layers" $
    mapM_
      ( \(a, b, c) -> do
          (maximum (map row [a, b, c]) - minimum (map row [a, b, c])) `shouldBe` 1
          (maximum (map column [a, b, c]) - minimum (map column [a, b, c])) `shouldBe` 1
      )
      ts

isValid :: LayerSettings -> Bool
isValid = either (const False) (const True) . validateSettings

closeTo :: Float -> Float -> Bool
closeTo expected actual = abs (expected - actual) < 1e-3

closeToPoint :: Vector -> Vector -> Bool
closeToPoint (x, y) (x', y') = closeTo x x' && closeTo y y'

-- | A path of between 2 and 30 points.
newtype PathOfAtLeast2 = PathOfAtLeast2 Path deriving (Show)

instance Arbitrary PathOfAtLeast2 where
  arbitrary = PathOfAtLeast2 <$> (choose (2, 30) >>= \n -> vectorOf n point)
    where
      point = (,) <$> choose (-100, 100) <*> choose (-100, 100)
