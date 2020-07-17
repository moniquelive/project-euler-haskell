module Lib
  ( pCurrent
  ) where

import Data.List
import Data.List.Split
import Data.Char(digitToInt)
import Data.Numbers.Primes

memoized_fib :: Int -> Int
memoized_fib = (map fib [0 ..] !!)
   where fib 0 = 0
         fib 1 = 1
         fib n = memoized_fib (n-2) + memoized_fib (n-1)

prime_factors :: Int -> [Int]
prime_factors 1 = []
prime_factors n
  | factors == [] = [n]
  | otherwise = factors ++ prime_factors (n `div` (head factors))
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2..n-1]

is_pal :: Int -> Bool
is_pal n = (even . length) s && s == reverse s
  where s = show n

combinations :: Int -> [a] -> [[a]]
combinations k ns = filter ((k==).length) (subsequences ns)

p1 :: Int
p1 = sum $ filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0) [1..1000-1]

p2 :: Int
p2 = sum $ filter even $ takeWhile (< 4000000) (map memoized_fib [1..])

p3 :: Int
p3 = maximum $ prime_factors 600851475143

p6 :: Int
p6 = sq_of_sum - sum_of_sq
  where sq_of_sum = (sum [1..100]) ^2
        sum_of_sq = sum $ map (^ 2) [1..100]

p5 :: Int
p5 = foldl lcm 1 [1..20]

p4 :: Int
-- p4 = fromJust $ find is_pal [ i * j | i <- [100..999], j <- [i..999] ]
p4 = maximum $ filter is_pal [ i * j | i <- [100..999], j <- [i..999] ]

p7 :: Integer
p7 = last $ take 10001 $ primes

p9 :: Integer
p9 = head $ [ i*j*k | i <- [1..1000], j <- [1..1000], k <- [1..1000],
                      i+j+k == 1000,
                      i*i+j*j==k*k]

slideNDigits :: Int -> String -> [String]
slideNDigits n d
  | length d < n = []
  | otherwise = take n d : (slideNDigits n $ tail d)

p8 :: Int
p8 = maximum
  $ map (product . map digitToInt)
  $ slideNDigits 13 digits
  where
    digits = "73167176531330624919225119674426574742355349194934\
\96983520312774506326239578318016984801869478851843\
\85861560789112949495459501737958331952853208805511\
\12540698747158523863050715693290963295227443043557\
\66896648950445244523161731856403098711121722383113\
\62229893423380308135336276614282806444486645238749\
\30358907296290491560440772390713810515859307960866\
\70172427121883998797908792274921901699720888093776\
\65727333001053367881220235421809751254540594752243\
\52584907711670556013604839586446706324415722155397\
\53697817977846174064955149290862569321978468622482\
\83972241375657056057490261407972968652414535100474\
\82166370484403199890008895243450658541227588666881\
\16427171479924442928230863465674813919123162824586\
\17866458359124566529476545682848912883142607690042\
\24219022671055626321111109370544217506941658960408\
\07198403850962455444362981230987879927244284909188\
\84580156166097919133875499200524063689912560717606\
\05886116467109405077541002256983155200055935729725\
\71636269561882670428252483600823257530420752963450"

p10 :: Integer
p10 = sum $ takeWhile (< 2000000) primes

pCurrent = p10
