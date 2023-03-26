module Problems.P14 where

-- 837799
p14 :: Int
p14 = snd . maximum $ zip (map (collatz 0) [1 .. 1000000]) [1 .. 1000000]
  where
    collatz :: Int -> Int -> Int
    collatz _ 0 = error "Impossibru"
    collatz l 1 = 1 + l
    collatz l n = collatz (l + 1) (nxt n)
      where
        nxt :: Int -> Int
        nxt i
          | even i = div i 2
          | otherwise = 3 * i + 1
