module Problems.P4 where

import Common (isPal)

p4 :: Int
p4 = maximum . filter isPal $ [i * j | i <- [100 .. 999], j <- [i .. 999]]
