module Problems.P2 where

import Common (fibs)

p2 :: Integer
p2 = sum . filter even . takeWhile (< 4000000) $ fibs
