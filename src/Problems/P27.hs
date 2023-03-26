module Problems.P27 where

import Data.Numbers.Primes (isPrime)

-- -59231
p27 :: Int
p27 =
  snd
    . maximum
    $ [ (len, a * b)
        | a <- [-999 .. 999],
          b <- [-1000 .. 1000],
          let f n = n * n + a * n + b,
          let len = length . takeWhile isPrime . map f $ [0 .. 100000],
          len > 1
      ]
