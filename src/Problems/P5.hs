module Problems.P5 where

p5 :: Int
p5 = foldl lcm 1 [1 .. 20]
