module Problems.P19 where

import qualified Data.Time as D

-- 171
p19 :: Int
p19 = length . filter f $ [start .. end]
  where
    start = D.fromGregorian 1901 1 1
    end = D.fromGregorian 2000 12 31
    f d = (D.dayOfWeek d == D.Sunday) && (dayOfMonth . D.toGregorian $ d) == 1
    dayOfMonth (_, _, dd) = dd
