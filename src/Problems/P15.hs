module Problems.P15 where

import Common (factorial)

-- 137846528820
p15 :: Integer
p15 = binomial (20 + 20) 20
  where
    binomial n k = factorial n `div` factorial k `div` factorial (n - k)
