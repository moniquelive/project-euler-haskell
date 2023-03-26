module Problems.P7 where

import Data.Numbers.Primes

p7 :: Integer
p7 = last . take 10001 $ primes
