module Obj (makeObj) where

makeObj :: [([Float, Float, Float])] -> String
makeObj rows = (concat.concat) $ 

map 
                                    (\(row) 
                                        -> (map.map (makeVectorString)  row ) 

makeVectorString ::  (Float,Float,Float)->String
makeVectorString (x,y,z) = printf "v %0.10f %0.10f %0.10f\n" x y z
