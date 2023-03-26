module Problems.P30 where

import Data.Char (ord)

-- 443839
p30 :: Int
p30 = sum $ filter f [10 .. 999999]
  where
    digits n = map (subtract 48 . ord) $ show n
    f n = n == (sum . map (^ (5 :: Int)) $ digits n)
