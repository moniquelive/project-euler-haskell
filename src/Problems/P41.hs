module Problems.P41 where

import Data.Char (ord)
import Data.List.Split (splitOn)

-- 162
p41 :: IO Int
p41 =
  length
    . filter (isTri . sum . map (subtract 64 . ord))
    . splitOn ","
    . filter (/= '"')
    <$> readFile "p42.txt"
  where
    triangular x = x * (x + 1) `div` 2
    isTri n = (== n) . head . dropWhile (< n) . map triangular $ [1 ..]
