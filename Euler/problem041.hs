#!/usr/bin/runhaskell

import Char
import Data.List

main :: IO ()
main = print(soln)

soln = head $ concatMap pandigs [9,8..1]
 where pandigs n = take 1 $ reverse $ sort $ filter isPrime $ map undigs $ permutations [1..n]
       
       primes = 2:(filter isPrime [3,5..])
       isPrime n = not $ isMultAny n (takeWhile (\x->x*x<=n) primes)
       isMultAny x (n:ns) = x`mod`n==0 || isMultAny x ns
       isMultAny x [] = False

       digs n = map digitToInt (show n)
       undigs xs = toInteger $ read $ map intToDigit xs

--isPandN xs = let n = length xs in and [elem d xs | d<-[1..n]]
--ds = (+) 1 . floor . logBase 10 . fromInteger

