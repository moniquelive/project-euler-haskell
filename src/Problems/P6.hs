{-# LANGUAGE TypeApplications #-}

module Problems.P6 where

p6 :: Integer
p6 =
    let
        first_hundred :: [Integer]
        first_hundred = [1 .. 100]

        sq_of_sum = sum first_hundred ^ (2 :: Integer)
        sum_of_squares = sum (map (^ (2 :: Integer)) first_hundred)
     in
        sq_of_sum - sum_of_squares
