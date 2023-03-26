module Problems.P6 where

p6 :: Int
p6 = sq_of_sum - sum_of_squares
  where
    sq_of_sum = sum [1 .. 100] ^ (2 :: Integer)
    sum_of_squares = sum (map (^ (2 :: Integer)) [1 .. 100])
