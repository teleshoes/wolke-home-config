#!/usr/bin/runhaskell

import Data.List

main :: IO ()
--main = print(factor 13195)
main = print(head (sortBy (\x y -> if x>y then LT else GT) (factor 600851475143)))

switch :: (a -> b -> c) -> b -> a -> c
switch f y x = f x y

--factor :: Int -> [Int]
--factor x = filter ((switch isMultiple) x) (primesUnder x)

factor :: Int -> [Int]
factor x = factorh x (primesUnder x)
factorh 1 _ = []
factorh x [] = error "fuckup"
factorh x (p:ps) | isMultiple p x = p : (factorh (x `div` p) ps)
                 | otherwise = factorh x ps

primesUnder :: Int -> [Int]
primesUnder x = sieve [2..x]

sieve :: [Int] -> [Int]
sieve (x:xs) = x : sieve (filter (not . isMultiple x) xs)
sieve [] = []

isMultiple :: Int -> Int -> Bool
isMultiple n x = (x `mod` n) == 0

