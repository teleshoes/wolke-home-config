#!/usr/bin/runhaskell

--AKS Algorithm (Lenstra & Pomerance Improvements)
-- only, i apparently have no idea how to do polynomial factorization
--module AKS where

import Data.List
import Data.Function
import Data.Maybe

main :: IO ()
main = print(take 10 aks_primes)

aks_primes = filter aks [2..] :: [Integer]

aks :: Integer -> Bool
aks n | n<=1 = False
      | isPower n = False --step 1
      | otherwise = let r = findR n --step 2
                    in not (maybeCompositeStep3 n r) --step 3
                       &&
                       isPrimeStep4 n r --step 4

--step 1
--find if n is a perfect power,
--  i.e.: n = a^b for an integer a and an integer b > 1
--perfect powers are obviously composite
isPower n =
  let as = [2..(floor $ sqrt $ fromInteger n)]
      in n`elem`concatMap (\a->takeWhile (<=n) $ iterate (*a) a) as

--step 2
--find the smallest r such that the multiplicative order of n (modulo r)
-- is greater than log.log n
findR n = head $ dropWhile (\r -> r<=1 || (ord r n) <= rLimit) (coprimes n)
          where rLimit = floor $ log2 $ log2 $ fromInteger n

--step 3
--if (gcd a n) != 1 for all a<=r, the n is composite.
-- take my word for it.
maybeCompositeStep3 n r = all (\a->gcd a n /= 1) [2..r]

--step 4
isPrimeStep4 n r =
  let aLimit = floor ((sqrtInt r)*(log2Int n))
      f x = x^r-1
      in False

totient n = length $ filter (coprime n) [1..n]
coprime a b = gcd a b == 1
coprimes n = filter (coprime n) [1..]

--ord n a is the multiplicative order of a (modulo n)
ord n a = head $ dropWhile (\k -> a^k`mod`n /= 1) [1..]

log2 = logBase 2

sqrtInt = sqrt.fromInteger
log2Int = log2.fromInteger


--syntactic sugar for a<x<b
-- a <~ x ~< b
a <~ x | a<x = Just x
       | otherwise = Nothing
(Just x) ~< b = x<b
Nothing ~< _ = False

