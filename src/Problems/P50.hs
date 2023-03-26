module Problems.P50 where

import Data.Numbers.Primes (isPrime, primes)

-- p50 :: Int
p50 :: Integer
p50 =
  let smallPrimes = takeWhile (< (50000 :: Integer)) primes
      consecutivePrimesFrom p = [(i, q) | (i, q) <- zip [0 ..] (cumsums smallPrimes), isPrime q]
        where
          cumsums = takeWhile (< (1000000 :: Integer)) . scanl1 (+) . dropWhile (< p)
   in snd . maximum $ [iq | p <- smallPrimes, iq <- consecutivePrimesFrom p]
