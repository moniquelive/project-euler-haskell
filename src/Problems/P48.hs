{-# LANGUAGE TypeApplications #-}

module Problems.P48 where

-- 9110846700A
p48 :: String
p48 =
  reverse . take 10 . reverse . show @Integer . sum $
    map (\x -> x ^ x) [1 .. 1000]
