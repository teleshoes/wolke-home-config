#!/usr/bin/runhaskell

import Data.List

main :: IO ()
main = print(soln 10001)

soln n = primes !! (n-1)
 where primes = 2:(filter isPrime [3,5..])
       isPrime n = not $ isMultAny n (takeWhile (\x->x*x<=n) primes)
       isMultAny x (n:ns) = x`mod`n==0 || isMultAny x ns
       isMultAny x [] = False



--soln n = primes !! (n+1)--nthprime n 1 [2..]
-- where nthprime n i (x:xs) | n==i = x
--                           | otherwise = nthprime n (i+1) (strain x xs)
--       strain x = filter $ not . isMultiple x
--       isMultiple n x = (x `mod` n) == 0

primes' = sieve [2..]

sieve (x:xs) = let (primes, remaining) = span (<x^2) (x:xs)
               in primes ++ sieve (strain primes remaining)
strain (x:xs) ys = strain xs (filter (not.isMultiple x) ys)
strain [] ys = ys

isMultiple n x = (x `mod` n) == 0

