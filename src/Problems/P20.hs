module Problems.P20 where

import Common (factorial)
import Data.Char (digitToInt)

-- 648
p20 :: Int
p20 = sum . map digitToInt . show . factorial $ 100
