module Problems.P11 where

import Data.List (transpose)
import Data.List.Split (chunksOf)

prodEveryN :: Int -> [Int] -> [Int]
prodEveryN n d
  | length d < n = [1]
  | otherwise = product (take n d) : prodEveryN n (tail d)

-- 70600674
p11 :: IO Int
p11 = go . chunksOf 20 . map read . words <$> readFile "p11.txt"
  where
    go matrix = maximum [byLines, byColumns, byDiagonals]
      where
        byDiagonals = maximum . map (maximum . prodEveryN 4) $ diagonals matrix
        byColumns = maximum . map (maximum . prodEveryN 4) $ transpose matrix
        byLines = maximum . map (maximum . prodEveryN 4) $ matrix
        diagonals [] = []
        diagonals ([] : xss) = xss
        diagonals xss =
          zipWith
            (++)
            (map ((: []) . head) xss ++ repeat [])
            ([] : diagonals (map tail xss))
