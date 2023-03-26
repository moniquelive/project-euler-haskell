module Problems.P31 where

-- 73682
p31 :: Int
p31 =
  let change :: Int -> [Int] -> Int
      change cents coins = change' coins !! cents
        where
          change' = foldr addCoin (1 : repeat 0)
          addCoin c oldlist = newlist
            where
              newlist = take c oldlist ++ zipWith (+) newlist (drop c oldlist)
   in change 200 [200, 100, 50, 20, 10, 5, 2, 1]
