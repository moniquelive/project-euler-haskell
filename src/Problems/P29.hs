module Problems.P29 where

import Data.List (nub, sort)

-- 9183
p29 :: Int
p29 =
  length
    . nub
    . sort
    $ [a ^ b | a <- [(2 :: Integer) .. 100], b <- [(2 :: Integer) .. 100]]
