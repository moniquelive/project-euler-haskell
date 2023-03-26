module Problems.P8 where

import Data.Char (digitToInt)

everyN :: Int -> [a] -> [[a]]
everyN n d
  | length d < n = []
  | otherwise = take n d : everyN n (tail d)

-- 23514624000
p8 :: IO Int
p8 = maximum . map product . everyN 13 . map digitToInt . head . lines <$> readFile "p8.txt"
