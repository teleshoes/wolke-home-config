#!/usr/bin/runhaskell

import Data.List
import Data.Function (on)
import Char

main :: IO ()
main = print(soln 1000000)

soln n = countCircularPrimesUnder n
 where countCircularPrimesUnder n = sieveCount n [2..n] 0
       sieveCount lim (x:xs) acc
           | x^2<lim   = sieveCount lim (strain x xs) (if circPrime x then acc+1 else acc)
           | otherwise = acc + (length $ filter circPrime (x:xs))
       
       strain x = filter $ not . (`isMult`x)
       
       isPrime' p = ps p [2..(p-1)]
       ps n []                  = True
       ps n (x:xs) | x`isMult`n = False
                   | x^2<n      = ps n (strain x xs)
                   | otherwise  = not $ isMultAny n xs
       
       isMult x n = 0 == x`mod`n
       isMultAny x = or . map (x`isMult`)
        
       rotations x = rot [] (digits x)
       rot _ [] = []
       rot xs (y:ys) = undigits (y:(ys++xs)) : (rot (xs++[y]) ys)
       
       digits = map digitToInt . show
       undigits = toInteger . fromInteger . read . map intToDigit
       
       circPrime = and . map isPrime . rotations
       
       isPrime n = n>=2 && (not $ n `isMultAny` (primesUnder ((floorsqrt n)+1)))     
       floorsqrt = floor . sqrt . fromIntegral

       primesUnder x = takeWhile (<x) primes
       
       primes = sieve [2..]
       sieve (x:xs) = x:(strain x (sieve xs))
       sieve [] = []

