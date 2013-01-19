#!/usr/bin/runhaskell

import Data.List
import Data.Char (intToDigit, digitToInt)

main :: IO ()
main = print(soln 8)

soln n = readInt $ smallest $ head $ allFamsOfSize n
 where
   allFamsOfSize n = let allFams = (concatMap families primes)
                     in filter ((n==).familySize) allFams

   leading fam = (head fam) == '*'
   smallest fam = replaceAll '*' (if leading fam then '1' else '0') fam

   familySize fam =
     let zero = if leading fam then "" else "0"
         valid = "123456789"++zero
         members = map readInt [(replaceAll '*' n fam) | n<-valid]
     in length $ filter isPrime $ members


   families p = fams (show p) (show p)
   fams [] _ = []
   fams (d:ds) digs = (famsWithDig d digs)++(fams (removeAll d ds) digs)

   famsWithDig d digs | not $ elem d digs = []
                      | otherwise =
    let keep = replaceFirst d '*' digs
        skip = replaceFirst d 'x' digs
        fams1 = (famsWithDig d keep)
        fams2 = map (replaceAll 'x' d) (famsWithDig d skip)
    in keep : (fams1++fams2)

   removeAll a xs = filter (a/=) xs

   replaceFirst a b (x:xs) | x == a = b:xs
                           | otherwise = x:(replaceFirst a b xs)
   replaceAll a b xs = map (\x->if x == a then b else x) xs

   readInt str = read $ str :: Integer

   primes = 2:(filter isPrime [3,5..])
   isPrime n = not $ any (mult n) (takeWhile (\p->p*p<=n) primes)
   a `mult` b = a`mod`b==0

