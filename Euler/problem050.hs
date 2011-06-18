#!/usr/bin/runhaskell

import Data.List
import Data.Function

main :: IO ()
main = print(soln 1000000)

-- +2357
--  +357
--   +57
--    +7
--    -5
--   -35
--   -3
--  -235
--  -23
--  -2

soln n = longest n
 where
   longest n = let guessLen = snd (guess n)
                   limPs = limit n guessLen primes
               in fst $ head $ filter (isSoln n) $ subseqs limPs

   isSoln n (sum,len) = sum<n && isPrime sum

   --this finds an arbitrary answer to use as a guaranteed ceiling
   -- {we need look only for subsequences larger than guess,
   --  which means we only need to look at primes beneath a certain value}
   guess n = head $ filter (isSoln n) $ subseqs (take 500 primes)

   --returns a finite list of all the elements that a contiguous
   -- subsequence of ns whose length is greater than min could possibly contain
   limit n k ps | sum (take k ps) > n = take k ps
                | otherwise = (head ps) : limit n k (tail ps)

   --returns a list of the sum and length of all subsequences,
   --IN DESCENDING ORDER OF LENGTH
   --  instead of chopping off elements from the front and back,
   --  chops off elements from the front and the reverse of the front
   subseqs ns = ssex (sum ns) (length ns) (reverse ns) ns
   ssex sum _ _ _ | sum <= 0 = []
   ssex sum len [] [] = (sum, len):[]
   ssex sum len [] (n:ns) = (sum,len):(ssex (sum-n) (len-1) [] ns)
   ssex sum len (r:rs) [] = (sum,len):(ssex (sum-r) (len-1) rs [])
   ssex sum len (r:rs) (n:ns) =
     let sansN = ssex (sum-n) (len-1) (r:rs) ns --all subseqs-sans-n
         sansR = ssex (sum-r) (len-1) rs []     --all subseqs-sans-r that also start with n
     in (sum,len):(merge comp sansN sansR)

   comp = (flip compare) `on` snd

   merge :: (Ord a)=>(a->a->Ordering)->[a]->[a]->[a]
   merge cmp xs [] = xs
   merge cmp [] ys = ys
   merge cmp (x:xs) (y:ys) | cmp x y == LT = x:(merge cmp xs (y:ys))
                           | otherwise     = y:(merge cmp (x:xs) ys)

   primes = 2:(filter isPrime [3,5..])
   isPrime n | n<=1 = False
   isPrime n = not $ any (mult n) (takeWhile (\p->p*p<=n) primes)
   a `mult` b = a`mod`b==0





--  2          23
-- |3|        |21|
-- |5|        |19|
-- |7|________|17|
-- \___11__13___/









soln'' n = head $ filter (\x->x<n && isPrime x) $ allPrimeSums 0 (upper n)
--primeSum 1 6--consex [1..5] (reverse [1..5])--sum $ head $ filter (\x->sum x < n && (isPrime $ sum x)) $ consex $ primesSumUnder n [] primes
 where
   consex [] = []
   consex [x] = [[x]]
   consex xs = let ts = tails xs
               in (head ts) : merge longestFirst (tail ts) (consex $ init xs)

   allPrimeSums j k | j>=k = []
                    | otherwise = (primesum j k):(allPrimeSums (j+1) k)`smoosh`(allPrimeSums j (k-1))

   xs `smoosh` [] = xs
   [] `smoosh` ys = ys
   (x:xs) `smoosh` (y:ys) = x:y:(xs `smoosh` ys)

   primesum j k = sum $ take (k-j+1) $ drop j primes

   upper n = primesSumUnder n primes 0

   primesSumUnder n (p1:p2:ps) i | p2+p1 > n = i
                                 | otherwise = primesSumUnder n (p2:ps) (i+1)

   takeInitsLazy n xs = take n (inits xs)
   takeInitsEager n xs = init $ initsFinite (take n xs)
   initsFinite = reverse . map reverse . tails . reverse

   longestFirst = compare `on` (\x->0-(length x))

   merge :: (Ord a)=>(a->a->Ordering)->[a]->[a]->[a]
   merge cmp xs [] = xs
   merge cmp [] ys = ys
   merge cmp (x:xs) (y:ys) | cmp x y == LT = x:(merge cmp xs (y:ys))
                           | otherwise     = y:(merge cmp (x:xs) ys)

   range xs = (head xs, last xs)

   primes = 2:(filter isPrime [3,5..])
   isPrime n | n<=1 = False
   isPrime n = not $ any (mult n) (takeWhile (\p->p*p<=n) primes)
   a `mult` b = a`mod`b==0

