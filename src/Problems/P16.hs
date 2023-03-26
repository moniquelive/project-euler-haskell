module Problems.P16 where

import Data.Char (digitToInt)

-- 1366
p16 :: Int
p16 = sum . map digitToInt . show $ (2 :: Integer) ^ (1000 :: Integer)
