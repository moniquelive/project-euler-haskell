{-# LANGUAGE TypeApplications #-}

module Problems.P67 where

-- 7273
p67 :: IO Int
p67 =
  head . count . map (map (read @Int) . words) . lines
    <$> readFile "p067_triangle.txt"
  where
    count [] = []
    count [xs] = xs
    count (xs : xss) = zipWith (+) xs (zipWith max (init cs) (tail cs))
      where
        cs = count xss
