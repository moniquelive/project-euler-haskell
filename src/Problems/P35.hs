{-# LANGUAGE TypeApplications #-}

module Problems.P35 where

import Data.Numbers.Primes (isPrime)

-- 73682
p35 :: Int
p35 =
  let circular = all (isPrime . read @Int) . circle
      rotate = drop <> take
      circle xs = map (`rotate` xs) [1 .. length xs]
   in length . filter (circular . show) $ filter isPrime [1 .. 1000000 :: Int]
