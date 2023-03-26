module Problems.P10 where

import Data.Numbers.Primes

p10 :: Integer
p10 = sum . takeWhile (< 2000000) $ primes
