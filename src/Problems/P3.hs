module Problems.P3 where

import Data.Numbers.Primes

p3 :: Int
p3 = maximum . primeFactors $ 600851475143
