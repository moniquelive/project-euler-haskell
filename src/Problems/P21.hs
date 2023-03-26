module Problems.P21 where

-- 31626
p21 :: Int
p21 = sum $ [x | x <- [2 .. 10000], let b = d x in b /= x && d b == x]
  where
    d n = sum $ [x | x <- [1 .. (n - 1)], n `rem` x == 0]
