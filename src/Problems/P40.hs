{-# LANGUAGE TypeApplications #-}

module Problems.P40 where

import Data.Char (digitToInt)

-- 210
p40 :: Int
p40 =
  product
    . map (digitToInt . (digits !!))
    $ [0, 10 - 1, 100 - 1, 1000 - 1, 10000 - 1, 100000 - 1, 1000000 - 1]
  where
    digits = concatMap (show @Int) [1 ..]
