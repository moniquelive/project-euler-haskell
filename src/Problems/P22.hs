module Problems.P22 where

import Data.Char (ord)
import Data.List (sort)
import Data.List.Split (splitOn)

-- 871198282
p22 :: IO Int
p22 =
  sum
    . zipWith f [1 ..]
    . sort
    . splitOn ","
    . filter (/= '"')
    <$> readFile "names.txt"
  where
    f n w = n * (sum . map (subtract 64 . ord) $ w)
