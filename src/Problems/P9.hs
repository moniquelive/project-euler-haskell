module Problems.P9 where

p9 :: Integer
p9 =
  head
    [ i * j * k
      | i <- [1 .. 500],
        j <- [i .. 500],
        k <- [j .. 500],
        i + j + k == 1000,
        i * i + j * j == k * k
    ]
