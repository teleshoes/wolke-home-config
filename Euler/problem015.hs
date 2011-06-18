#!/usr/bin/runhaskell

import Data.List

main :: IO ()
main = print(soln1 20)


soln1 n = (2*n) `c` n


soln2 n = paths (n+1) (n+1)
paths _ 1 = 1
paths 1 _ = 1
paths x 2 = x
paths 2 y = y
paths x y | x==y      = 2*(paths (x-1) y)
          | otherwise = (paths (x-1) y) + (paths x (y-1))


--   6   5  4  3  2  1
--6 252
--5 126 70
--4 56  35 20
--3 21  15 10 06
--2 06  05 04 03 02
--1 01  01 01 01 01 01

--   4  3  2  1
--4 20 10 04 01
--3 10 06 03 01
--2 04 03 02 01
--1 01 01 01 01




c :: Integer -> Integer -> Integer
n `c` k = choose n (min k (n-k)) 1 1
 where choose n k np kp
        | k==0 = np `div` kp
        | otherwise = choose (n-1) (k-1) (n*np) (k*kp)

p :: Integer -> Integer -> Integer
n `p` r = product [(n-r+1)..n]

fac x = fach x 1
fach x prod | x==1 = prod | otherwise = (fach (x-1) prod*x)


