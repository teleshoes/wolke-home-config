#!/usr/bin/runhaskell

import Char
import Data.List
import Data.Maybe

main :: IO ()
main = print(soln)

soln = (map concTrip $ concatMap findSpecials permsOfP4D) !! 1
 where
   concTrip (x,y,z) = read (show x ++ show y ++ show z) :: Integer

   findSpecials perms = filter isSpecialArithSeq (map extrapolate $ pairs perms)

   buckets _ [] = []
   buckets f (x:xs) = let (good,bad) = partition (f x) xs
                      in (x:good):(buckets f bad)
   
   pairs [] = []
   pairs (x:xs) = (map (\y->(x,y)) xs) ++ (pairs xs)

   extrapolate (x,y) = (x,y,y+y-x)
   isSpecialArithSeq (x,y,z) = isP4D z && y-x == 3330 && z-y == 3330

   permsOfP4D = buckets isPerm prime4D
   isP4D x = elemSorted x prime4D
   isPerm x y = (sort $ show x) == (sort $ show y)
   prime4D = takeWhile (<10000) $ dropWhile (<1000) primes

   elemSorted x (y:ys) = let mx = (listToMaybe $ dropWhile (<x) ys)
                         in maybe False (x ==) mx

   primes = 2:(filter isPrime [3,5..])
   isPrime n = not $ any (mult n) (takeWhile (\p->p*p<=n) primes)
   a `mult` b = a`mod`b==0


--   intPermutations n = map (toInteger . read) $ (permutations $ show n)
--   perms n = let ps = sort $ nub $ filter isP4d $ intPermutations n
--             in if head ps == n then ps else []


--soln = doStuff (map seqs prime4d)
-- where
--   doStuff [] = []
--   doStuff (x:xs) = (diffs (head x) (tail x)) ++ (doStuff xs)
--
--   diffs x [] = []
--   diffs x (y:ys) | y-x==3330 && (elemSorted (y+3330) prime4d) = (x,y,y+3330):(diffs x ys)
--                  | otherwise = (diffs x ys)

