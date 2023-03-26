{-# LANGUAGE TypeApplications #-}

module Problems.P18 where

-- data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)
-- 1074
p18 :: IO Int
p18 = head . count . map (map (read @Int) . words) . lines <$> readFile "p18.txt"
  where
    count [] = []
    count [xs] = xs
    count (xs : xss) = zipWith (+) xs (zipWith max (init cs) (tail cs))
      where
        cs = count xss
