module Common (
    factorial,
    fib,
    fibs,
    isPal,
)
where

factorial :: Integer -> Integer
factorial n = product [1 .. n]

-- fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
fibs :: (Num a) => [a]
fibs = 0 : scanl (+) 1 fibs

fib :: (Num a) => Int -> a
fib n = fibs !! n

isPal :: (Show a) => a -> Bool
isPal n = (even . length) s && s == reverse s
  where
    s = show n
