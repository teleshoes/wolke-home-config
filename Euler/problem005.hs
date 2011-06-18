#!/usr/bin/runhaskell

import Data.List
import Data.Ord
import Data.Function (on)

main :: IO ()
main = print(soln)

soln = product $ map powpair $ map maxsnd $ groupBy (\x y -> (fst x)==(fst y)) $ sortfst primes
 where primes = (joinall $ map primefactors [2..20])
       sortfst = sortBy (compare `on` fst)
       powpair (x,y) = x^y
       maxsnd xs = maxsecond xs (-1,-1)
       maxsecond [] (amax,bmax) = (amax,bmax)
       maxsecond ((a,b):xs) (amax,bmax) | b > bmax = maxsecond xs (a,b)
                                        | otherwise = maxsecond xs (amax,bmax)
       joinall [] = []
       joinall (x:xs) = x++(joinall xs)

       smoosh [] = []
       smoosh (x:xs) = (head x, length x):smoosh xs

       primefactors x = smoosh $ group $ prime x (primesUnder x) []
       prime 1 _ fax = fax
       prime x [] fax = fax
       prime x (p:xs) fax | isMultiple p x = prime (x`div`p) (p:xs) (p:fax)
                          | otherwise = prime x xs fax                         

       primesUnder :: Int -> [Int]
       primesUnder x = sieve [2..x]

       sieve (x:xs) = x : sieve (filter (not . isMultiple x) xs)
       sieve [] = []

       isMultiple :: Int -> Int -> Bool
       isMultiple n x = (x `mod` n) == 0



--smallestMultAll :: Int -> Int
--smallestMultAll x = firstOcc (isMultipleAll [1..x]) [floor..]
--                    where floor = (prodList (primesUnder x))

--smallestMultAll :: Int -> Int
--smallestMultAll x = head (customSieve [1..x] [floor..])
--                    where floor = (prodList (primesUnder x))


--prodList :: [Int] -> Int
--prodList [] = 1
--prodList (x:xs) = x * prodList xs

--customSieve :: [Int] -> [Int] -> [Int]
--customSieve [] xs = xs
--customSieve (n:ns) xs = customSieve ns (filter (not . isMultiple n) xs)

--dropWhile' :: (a -> Bool) -> [a] -> [a]
--dropWhile' _ [] = []
--dropWhile' f (x:xs) | f x = dropWhile f xs
--                    | otherwise = (x:xs)

--firstOcc :: (a -> Bool) -> [a] -> a
--firstOcc f xs = head (dropWhile' (not . f) xs)

--isMultipleAll :: [Int] -> Int -> Bool
--isMultipleAll [] _ = True
--isMultipleAll (n:ns) x | isMultiple n x = isMultipleAll ns x
--                       | otherwise = False

