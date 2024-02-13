module Problems.P4 where

import Common (isPal)

p4 :: Int
p4 =
    let products = [i * j | i <- [100 .. 999], j <- [i .. 999]]
     in (maximum . filter isPal) products
