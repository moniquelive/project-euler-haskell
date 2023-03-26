{-# LANGUAGE TypeApplications #-}

module Problems.P12 where

import Data.List (findIndex)

-- 76576500 (4s!)
p12 :: Int
p12 =
  triangular
    . maybe 0 (+ 1)
    . findIndex (> 500)
    . map (factors . triangular)
    $ [1 ..]
  where
    factors :: Int -> Int
    factors n = 2 * fromIntegral y - 1
      where
        y = length [i | i <- [1 .. u], mod n i == 0]
        u :: Int
        u = round . sqrt @Double . fromIntegral $ n
    triangular x = x * (x + 1) `div` 2
