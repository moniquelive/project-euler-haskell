module Problems.P52 where

import Data.List (sort)

-- 142857
p52 :: Int
p52 = head . dropWhile (not . digits) $ [1 ..]
  where
    digits n = all (== head lst) (tail lst)
      where
        lst = map (sort . show) [n * 2, n * 3, n * 4, n * 5, n * 6]
