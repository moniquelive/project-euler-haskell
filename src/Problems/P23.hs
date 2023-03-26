module Problems.P23 where

import Data.Array (listArray, (!))
import Data.List (group, tails)
import Data.Numbers.Primes (primeFactors)

-- 4179871
p23 :: Int
p23 =
  let combinations xs ys = [x ++ y | x <- xs, y <- tails ys]
      divisors = map product . foldl combinations [[]] . (group . primeFactors)
      properDivisors = tail . divisors
      abundant y = y /= 0 && (sum (properDivisors y) > y)
      abundant' = listArray (0, 28123) $ map abundant [0 .. 28123 :: Int]
      sumOf2 f n = or [f ! x && f ! (n - x) | x <- [0 .. n `div` 2]]
   in sum . filter (not . sumOf2 abundant') $ [1 .. 28123 :: Int]
