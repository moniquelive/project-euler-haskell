{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Lib where

import           Control.Monad                  ( liftM2 )
import           Data.List
import           Data.List.Split
import           Data.Char                      ( digitToInt
                                                , intToDigit
                                                , ord
                                                )
import           Data.Numbers.Primes
import           Data.Maybe
import qualified Data.Time                     as D
import qualified Data.IntSet                   as S
import           Numeric

memoizedFib :: Int -> Int
memoizedFib = (map fib [0 ..] !!)
 where
  fib 0 = 0
  fib 1 = 1
  fib n = memoizedFib (n - 2) + memoizedFib (n - 1)

isPal :: (Show a) => a -> Bool
isPal n = (even . length) s && s == reverse s where s = show n

-- combinations :: Int -> [a] -> [[a]]
-- combinations k ns = filter ((k ==) . length) (subsequences ns)

p1 :: Int
p1 = sum . filter f $ [1 .. 1000 - 1]
  where f x = 0 `elem` [x `mod` 3, x `mod` 5]

p2 :: Int
p2 = sum . filter even . takeWhile (< 4000000) . map memoizedFib $ [1 ..]

p3 :: Int
p3 = maximum . primeFactors $ 600851475143

p6 :: Int
p6 = sq_of_sum - sum_of_squares
 where
  sq_of_sum      = sum [1 .. 100] ^ (2 :: Integer)
  sum_of_squares = sum (map (^ (2 :: Integer)) [1 .. 100])

p5 :: Int
p5 = foldl lcm 1 [1 .. 20]

p4 :: Int
p4 = maximum . filter isPal $ [ i * j | i <- [100 .. 999], j <- [i .. 999] ]

p7 :: Integer
p7 = last . take 10001 $ primes

p9 :: Integer
p9 = head
  [ i * j * k
  | i <- [1 .. 1000]
  , j <- [1 .. 1000]
  , k <- [1 .. 1000]
  , i + j + k == 1000
  , i * i + j * j == k * k
  ]

everyN :: Int -> [a] -> [[a]]
everyN n d | length d < n = []
           | otherwise    = take n d : everyN n (tail d)

-- 23514624000
p8 :: IO Int
p8 = maximum . map product . everyN 13 . map digitToInt . head . lines <$> readFile "p8.txt"

p10 :: Integer
p10 = sum . takeWhile (< 2000000) $ primes

prodEveryN :: Int -> [Int] -> [Int]
prodEveryN n d | length d < n = [1]
               | otherwise    = product (take n d) : prodEveryN n (tail d)

-- 70600674
p11 :: IO Int
p11 = go . chunksOf 20 . map read . words <$> readFile "p11.txt"
 where
  go matrix = maximum [byLines, byColumns, byDiagonals]
    where
      byDiagonals = maximum . map (maximum . prodEveryN 4) $ diagonals matrix
      byColumns   = maximum . map (maximum . prodEveryN 4) $ transpose matrix
      byLines     = maximum . map (maximum . prodEveryN 4) $ matrix
      diagonals []         = []
      diagonals ([] : xss) = xss
      diagonals xss        = zipWith (++)
                                    (map ((: []) . head) xss ++ repeat [])
                                    ([] : diagonals (map tail xss))

-- 1366
p16 :: Int
p16 = sum . map digitToInt . show $ (2 :: Integer) ^ (1000 :: Integer)

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
    nxt i | even i    = div i 2
          | otherwise = 3 * i + 1

-- 5537376230
p13 :: IO Integer
p13 = read . take 10 . show . sum . map (read @Integer) . words <$> readFile "p13.txt"

-- 76576500 (4s!)
p12 :: Int
p12 =
  triangular
    . maybe 0 (+ 1)
    . findIndex (> 500)
    . map (factors . triangular)
    $ [1 ..]
 where
  factors :: Int -> Int
  factors n = 2 * fromIntegral y - 1
   where
    y = length [ i | i <- [1 .. u], mod n i == 0 ]
    u :: Int
    u = round . sqrt @Double . fromIntegral $ n
  triangular x = x * (x + 1) `div` 2

factorial :: Integer -> Integer
factorial n = product [1 .. n]

-- 648
p20 :: Int
p20 = sum . map digitToInt . show . factorial $ 100

-- 137846528820
p15 :: Integer
p15 = binomial (20 + 20) 20
  where binomial n k = factorial n `div` factorial k `div` factorial (n - k)

-- 4782
p25 :: Int
p25 = fromMaybe 0 . findIndex (> (10 :: Integer) ^ (999 :: Integer)) $ fibs
  where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- 21124
p17 :: Int
p17 = length . concatMap decompose $ [1 .. 1000]
 where
  one =
    [ "one"
    , "two"
    , "three"
    , "four"
    , "five"
    , "six"
    , "seven"
    , "eight"
    , "nine"
    , "ten"
    , "eleven"
    , "twelve"
    , "thirteen"
    , "fourteen"
    , "fifteen"
    , "sixteen"
    , "seventeen"
    , "eighteen"
    , "nineteen"
    ]
  ty =
    [ "twenty"
    , "thirty"
    , "forty"
    , "fifty"
    , "sixty"
    , "seventy"
    , "eighty"
    , "ninety"
    ]
  decompose x
    | x == 0 = []
    | x < 20 = one !! (x - 1)
    | x >= 20 && x < 100 = ty !! (firstDigit x - 2) ++ decompose
      (x `mod` 10 :: Int)
    | x < 1000 && x `mod` 100 == 0 = one !! (firstDigit x - 1) ++ "hundred"
    | x > 100 && x <= 999 =  one
    !! (firstDigit x - 1)
    ++ "hundredand"
    ++ decompose (x `mod` 100 :: Int)
    | x == 1000 = "onethousand"
    | otherwise = error "Impossibru"
    where firstDigit = digitToInt . head . show

-- 31626
p21 :: Int
p21 = sum $ [ x | x <- [2 .. 10000], let b = d x in b /= x && d b == x ]
  where d n = sum $ [ x | x <- [1 .. (n - 1)], n `rem` x == 0 ]

-- data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)
-- 1074
p18 :: IO Int
p18 = head . count . map (map (read @Int) . words) . lines <$> readFile "p18.txt"
 where
  count []         = []
  count [xs      ] = xs
  count (xs : xss) = zipWith (+) xs (zipWith max (init cs) (tail cs))
    where cs = count xss

-- 171
p19 :: Int
p19 = length . filter f $ [start .. end]
 where
  start = D.fromGregorian 1901 1 1
  end   = D.fromGregorian 2000 12 31
  f d = (D.dayOfWeek d == D.Sunday) && (dayOfMonth . D.toGregorian $ d) == 1
  dayOfMonth (_, _, dd) = dd

-- 871198282
p22 :: IO Int
p22 =
  sum
    .   zipWith f [1 ..]
    .   sort
    .   splitOn ","
    .   filter (/= '"')
    <$> readFile "names.txt"
 where
  f n w = n * (sum . map (subtract 64 . ord) $ w)

-- 2783915460
p24 :: String
p24 = permu "0123456789" !! (1000000 - 1)
 where
  permu :: [a] -> [[a]]
  permu []  = [[]]
  permu xxs = [ y : zs | (y, ys) <- select xxs, zs <- permu ys ]
   where
    select []       = []
    select (x : xs) = (x, xs) : [ (y, x : ys) | (y, ys) <- select xs ]

-- 9110846700A
p48 :: String
p48 = reverse . take 10 . reverse . show @Integer . sum $ map (\x -> x ^ x)
                                                              [1 .. 1000]

-- 443839
p30 :: Int
p30 = sum $ filter f [10 .. 999999]
 where
  digits n = map (subtract 48 . ord) $ show n
  f n = n == (sum . map (^ (5 :: Int)) $ digits n)

-- 669171001
p28 :: Int
p28 = (+ 1) . sum $ map diags [1 .. n 1001]
 where
  n x = (x - 1) `div` 2
  diags x =
    (4 * x * x + 4 * x + 1)
      + (4 * x * x + 1)
      + (4 * x * x - 2 * x + 1)
      + (4 * x * x + 2 * x + 1)

-- 9183
p29 :: Int
p29 =
  length
    . nub
    . sort
    $ [ a ^ b | a <- [(2 :: Integer) .. 100], b <- [(2 :: Integer) .. 100] ]

isqrt :: Int -> Int
isqrt n = floor (sqrt @Double $ fromIntegral n)

-- 4179871
p23 :: Int
p23 = sum . S.elems $ S.difference (S.fromList [1 .. 28123 - 1 :: Int]) sums
 where
  sums = S.unions . map S.fromList . zipWith (map . (+)) abundants $ tails abundants
  abundants = filter divisors [12 .. 28123 - 1 :: Int]
  divisors n
    | n < 4 = False
    | otherwise = (> n) . (+ 1) . sum $ [ s + t | i <- [2 .. isqrt n]
        , let (q, r) = n `quotRem` i
        , let s      = if r == 0 then i else 0
        , let t      = if r == 0 && q /= i then q else 0 ]

-- 7273
p67 :: IO Int
p67 = head . count . map (map (read @Int) . words) . lines <$>
  readFile "p067_triangle.txt"
 where
  count []         = []
  count [xs      ] = xs
  count (xs : xss) = zipWith (+) xs (zipWith max (init cs) (tail cs))
    where cs = count xss


-- 40730
p34 :: Int
p34 = sum . filter (\n -> n == digitsFact n) $ [3 .. 100000]
 where
  digitsFact = sum . map (fact . digitToInt) . show
  fact n = product [2 .. n]

-- 872187
andP :: (a -> Bool) -> (a -> Bool) -> a -> Bool
andP = liftM2 (&&)

p36 :: Int
p36 = sum . filter (andP pal2 pal10) $ [1 .. 1000000]
 where
  pal10 n = let s = show n in s == reverse s
  pal2 n = let s = showIntAtBase 2 intToDigit n "" in s == reverse s

-- -59231
p27 :: Int
p27 =
  snd
    . maximum
    $ [ (len, a * b)
      | a <- [-999 .. 999]
      , b <- [-1000 .. 1000]
      , let f n = n * n + a * n + b
      , let len = length . takeWhile isPrime . map f $ [0 .. 100000]
      , len > 1
      ]

-- 73682
p31 :: Int
p31 = length
  [ 1 :: Int
  | a <- [0 .. 200 `div` 1 :: Int]
  , b <- [0 .. 200 `div` 2 :: Int]
  , c <- [0 .. 200 `div` 5 :: Int]
  , d <- [0 .. 200 `div` 10 :: Int]
  , e <- [0 .. 200 `div` 20 :: Int]
  , f <- [0 .. 200 `div` 50 :: Int]
  , g <- [0 .. 200 `div` 100 :: Int]
  , h <- [0 .. 200 `div` 200 :: Int]
  , 1 * a + 2 * b + 5 * c + 10 * d + 20 * e + 50 * f + 100 * g + 200 * h == 200
  ]

-- 73682
p35 :: Int
p35 = length . filter (circular . show) $ [1 .. 1000000 :: Int]
 where
  circular = all (isPrime . read @Int) . circle
  circle xs =
    let trim ys = zipWith const ys xs
    in  trim . map trim . iterate tail . cycle $ xs

-- 983
p26 :: Int
p26 =
  snd
    . maximum
    $ [ (cycleLength co10 tenModCo10 tenModCo10 1, n)
      | n <- [3 .. 1000]
      , let co10       = coPrime10 n
      , let tenModCo10 = 10 `rem` co10
      , co10 /= 1
      ]
 where
  coPrime10 = product . filter (andP (/= 2) (/= 5)) . primeFactors
  cycleLength :: Int -> Int -> Int -> Int -> Int
  cycleLength n mv cm k | cm == 1   = k
                        | otherwise = cycleLength n mv (cm * mv `rem` n) (k + 1)

-- 210
p40 :: Int
p40 =
  product
    . map (digitToInt . (digits !!))
    $ [0, 10 - 1, 100 - 1, 1000 - 1, 10000 - 1, 100000 - 1, 1000000 - 1]
  where digits = concatMap (show @Int) [1 ..]

-- 162
p41 :: IO Int
p41 =
  length
    .   filter (isTri . sum . map (subtract 64 . ord))
    .   splitOn ","
    .   filter (/= '"')
    <$> readFile "p42.txt"
 where
  triangular x = x * (x + 1) `div` 2
  isTri n = (== n) . head . dropWhile (< n) . map triangular $ [1 ..]

pCurrent :: IO Int
pCurrent = undefined

