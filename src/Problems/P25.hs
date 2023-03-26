module Problems.P25 where

-- 4782
p25 :: Int
p25 = ceiling ((k + logBase 10 5 / 2 - 1) / logBase 10 phi)
  where
    k = 1000 :: Double
    phi = (1 + sqrt 5) / 2
