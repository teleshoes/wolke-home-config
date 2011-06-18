#!/usr/bin/runhaskell

import Data.List
import Data.Function

main :: IO ()
main = print(soln)

--[18433,12409,2341,1237,7] => 34427
--[8389,6733,5701,5197,13] => 26033

soln = smallestFamily
 where
   primes = 2:(filter isPrime [3,5..])
   isPrime n = n>1 && (not $ any (mult n) (takeWhile (\p->p*p<=n) primes))
   a`mult`b = a`mod`b==0

   famsize = 5
   smallestKnownFamSum = guess

   ceilGuesses = iterate (*4) 2000
   guess = head $ concatMap allFamiliesUnder ceilGuesses

   smallestFamily = smallestFamilyUnder smallestKnownFamSum
   smallestFamilyUnder n = head $ sort $ allFamiliesUnder n

   allFamiliesUnder n = map sum $ map fst $ select famsize primes n


   select 0 xs _ = [([],xs)]
   select i xs ceil = concatMap (selNext ceil) (select (i-1) xs ceil)

   pairables p ps = filter (\p2->primePair (p,p2)) ps

   selNext ceil (xs,ns) = selNexth ceil (xs,takeWhile (<=ceil) ns)
   selNexth ceil (xs,ns)
     | let min = sum $ take (famsize-(length xs)) ns
           in min+(sum xs) > ceil = []
   selNexth _ (_,[]) = []
   selNexth ceil (xs,(n:ns)) =
     (n:xs,pairables n ns):(selNexth ceil (xs,ns))
   
   primePair (x,y) = isPrime (x`concatDs`y) && isPrime (y`concatDs`x)

   concatDs x y = x*(10^(digsin y)) + y

   digsin x = length $ takeWhile (>0) $ iterate (`div`10) x



--   primeCombo ps = and $ map primePair $ allPairs ps

--   allPairs [] = []
--   allPairs (x:xs) = map (\y->(x,y)) xs ++ allPairs xs

--   minSum = compare `on` (sum.fst)

--   smoosh (x:xs) (y:ys) = x:y:(smoosh xs ys)

--   spanLTE cmp x ys = span (\y->y`cmp`x /= GT) ys

--   mergeAllFinite cmp [] = []
--   mergeAllFinite cmp [xs] = xs
--   mergeAllFinite cmp (xs:ys:xss) =
--     mergeAllFinite cmp ((merge cmp xs ys):xss)

--   mergeAll cmp xss = mAll cmp (filter (not.null) xss)
--   mAll _ [] = []
--   mAll _ [xs] = xs
--   mAll cmp (xs:ys:[]) = merge cmp xs ys
--   mAll cmp (xs:ys:zs:xss) =
--     let minZ = head zs
--         xsys = merge cmp xs ys
--         (good, rest) = spanLTE cmp minZ xsys
--         in good ++ mAll cmp (rest:zs:xss)

--   merge :: (Ord a)=>(a->a->Ordering)->[a]->[a]->[a]
--   merge cmp xs [] = xs
--   merge cmp [] ys = ys
--   merge cmp (x:xs) (y:ys) | x`cmp`y == LT = x:(merge cmp xs (y:ys))
--                           | otherwise     = y:(merge cmp (x:xs) ys)

