{-# LANGUAGE TypeApplications #-}

module Problems.P13 where

-- 5537376230
p13 :: IO Integer
p13 = read . take 10 . show . sum . map (read @Integer) . words <$> readFile "p13.txt"
