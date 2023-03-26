module Problems.P2 where

import Common (fib)

p2 :: Integer
p2 = sum . filter even . takeWhile (< 4000000) . map fib $ [1 ..]
