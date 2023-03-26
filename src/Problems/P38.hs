module Problems.P38 where

import Data.List (sort)

-- 932718654
p38 :: Int
p38 =
  let x num n str
        | length str >= 9 = str
        | otherwise = x num (n + 1) (str ++ show (num * n))
   in maximum [read s | num <- [1 .. 10000] :: [Int], let s = x num 1 "", sort s == "123456789"]
