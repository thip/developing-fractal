-- | Turns layers into a triangle mesh, and writes it in Wavefront OBJ format.
module Obj (Mesh (..), Vertex, Triangle, meshFromLayers, renderObj) where

import Text.Printf (printf)

import Layers (Layers, layerPaths)
import Vector (Path, scaleVector)

type Vertex = (Float, Float, Float)

-- | Three indices into the mesh's vertices, counting from 1 as OBJ does.
type Triangle = (Int, Int, Int)

data Mesh = Mesh
  { vertices :: [Vertex]
  , triangles :: [Triangle]
  }
  deriving (Show)

meshScale, baseLayerHeight :: Float
meshScale = 10
baseLayerHeight = 10

meshFromLayers :: Layers -> Mesh
meshFromLayers layers = Mesh (layerVertices paths) (joinLayers (length paths) width)
  where
    paths = layerPaths layers
    width = maximum (0 : map length paths)

-- | Places each layer at twice the height of the one before.
layerVertices :: [Path] -> [Vertex]
layerVertices = concat . zipWith layer [0 :: Int ..]
  where
    layer order = map (place (baseLayerHeight * 2 ^ order))
    place z point = let (x, y) = scaleVector meshScale point in (x, y, z)

-- | Joins each point to its neighbours on the same layer and the next one, for a
-- given number of layers of a given width.
joinLayers :: Int -> Int -> [Triangle]
joinLayers height width = concatMap quad topCorners
  where
    quad p = [(a, b, c), (b, d, c)]
      where
        (a, b, c, d) = (p, p + 1, p + width, p + width + 1)
    topCorners = [x + y | x <- [0 .. width - 2], y <- layerStarts]
    layerStarts = [1 + n * width | n <- [0 .. height - 2]]

renderObj :: Mesh -> String
renderObj (Mesh vs ts) = concatMap vertexLine vs ++ concatMap faceLine ts

vertexLine :: Vertex -> String
vertexLine (x, y, z) = printf "v %0.10f %0.10f %0.10f\n" x y z

faceLine :: Triangle -> String
faceLine (a, b, c) = printf "f %d %d %d\n" a b c
