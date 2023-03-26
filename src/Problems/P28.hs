module Problems.P28 where

-- 669171001
p28 :: Int
p28 = (+ 1) . sum $ map diags [1 .. n 1001]
  where
    n x = (x - 1) `div` 2
    diags x =
      (4 * x * x + 4 * x + 1)
        + (4 * x * x + 1)
        + (4 * x * x - 2 * x + 1)
        + (4 * x * x + 2 * x + 1)
