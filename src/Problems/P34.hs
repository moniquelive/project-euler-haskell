module Problems.P34 where

import Data.Char (digitToInt)

-- 40730
p34 :: Int
p34 = sum . filter (\n -> n == digitsFact n) $ [3 .. 100000]
  where
    digitsFact = sum . map (fact . digitToInt) . show
    fact n = product [2 .. n]
