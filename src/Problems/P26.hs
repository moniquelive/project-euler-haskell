module Problems.P26 where

import Data.Numbers.Primes (primeFactors)

-- 983
p26 :: Int
p26 =
  snd
    . maximum
    $ [ (cycleLength co10 tenModCo10 tenModCo10 1, n)
        | n <- [3 .. 1000],
          let co10 = coPrime10 n,
          let tenModCo10 = 10 `rem` co10,
          co10 /= 1
      ]
  where
    coPrime10 = product . filter (and . sequence [(/= 2), (/= 5)]) . primeFactors
    cycleLength :: Int -> Int -> Int -> Int -> Int
    cycleLength n mv cm k
      | cm == 1 = k
      | otherwise = cycleLength n mv (cm * mv `rem` n) (k + 1)
