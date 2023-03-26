module Problems.P1 where

p1 :: Int
p1 = sum . filter f $ [1 .. 1000 - 1]
  where
    f x = 0 `elem` [x `mod` 3, x `mod` 5]
