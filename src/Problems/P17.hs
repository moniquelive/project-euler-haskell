module Problems.P17 where

import Data.Char (digitToInt)

-- 21124
p17 :: Int
p17 = length . concatMap decompose $ [1 .. 1000]
  where
    one =
      [ "one",
        "two",
        "three",
        "four",
        "five",
        "six",
        "seven",
        "eight",
        "nine",
        "ten",
        "eleven",
        "twelve",
        "thirteen",
        "fourteen",
        "fifteen",
        "sixteen",
        "seventeen",
        "eighteen",
        "nineteen"
      ]
    ty =
      [ "twenty",
        "thirty",
        "forty",
        "fifty",
        "sixty",
        "seventy",
        "eighty",
        "ninety"
      ]
    decompose x
      | x == 0 = []
      | x < 20 = one !! (x - 1)
      | x >= 20 && x < 100 =
          ty !! (firstDigit x - 2)
            ++ decompose
              (x `mod` 10 :: Int)
      | x < 1000 && x `mod` 100 == 0 = one !! (firstDigit x - 1) ++ "hundred"
      | x > 100 && x <= 999 =
          one
            !! (firstDigit x - 1)
            ++ "hundredand"
            ++ decompose (x `mod` 100 :: Int)
      | x == 1000 = "onethousand"
      | otherwise = error "Impossibru"
      where
        firstDigit = digitToInt . head . show
