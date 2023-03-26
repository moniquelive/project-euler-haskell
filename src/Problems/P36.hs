module Problems.P36 where

import Data.Char (intToDigit)
import Numeric (showIntAtBase)

-- 872187
p36 :: Int
p36 =
  let pal10 n = show n == reverse (show n)
      pal2 n = let bin = showIntAtBase 2 intToDigit n "" in bin == reverse bin
   in sum . filter pal2 . filter pal10 $ [1 .. 1000000]
