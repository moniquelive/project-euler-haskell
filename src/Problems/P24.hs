module Problems.P24 where

-- 2783915460
p24 :: String
p24 = permu "0123456789" !! (1000000 - 1)
  where
    permu :: [a] -> [[a]]
    permu [] = [[]]
    permu xxs = [y : zs | (y, ys) <- select xxs, zs <- permu ys]
      where
        select [] = []
        select (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- select xs]
