#!/usr/bin/runhaskell

import Data.List
import Data.Function (on)

main :: IO ()
main = print(soln 1000)

--b must be prime for n==0
--a+b+1 must be prime for n==1


soln range = (\(a,b)->a*b) $ maxcoef coefs (0, (0,0))
 where coefs = [(a,b) | a<-[(-1*range)..range], b<-(takeWhile (<=range) primes), (isPrime' (a+b+1))]
       quad (a,b) n = n^2 + a*n + b
       maxcoef [] (curmax,curcoef) = curcoef
       maxcoef (x:xs) (curmax,curcoef) | isPrime' (quad x curmax) =
                                           let len = maxnlen x 0
                                           in maxcoef xs (if len>curmax then (len,x) else (curmax,curcoef))
                                       | otherwise = maxcoef xs (curmax,curcoef)
       maxnlen (a,b) n | isPrime' (quad (a,b) n) = maxnlen (a,b) (n+1)
                       | otherwise = n

isPrime' n = n>=2 && (not $ n `isMultAny` (primesUnder ((floorsqrt n)+1)))

primesUnder x = takeWhile (<x) primes


isPrime n | n<=1 = False
isPrime 2 = True
isPrime 3 = True
isPrime n = not(
             (isMultAny n ps) --small cache to handle most checks
             ||
             (isMult n 2) || (isMult n 3) || (isMultAny n kp) || (isMultAny n km)
            )
 where fl = floorsqrt n
       ps = (takeWhile (<=fl) someprimes)
       kp = (takeWhile (<=fl) kplus)
       km = (takeWhile (<=fl) kminus)

someprimes = take 500 primes
kplus = [(6*k+1) | k<-[1..]]
kminus = [(6*k-1) | k<-[1..]]


primes = sieve [2..]
sieve (x:xs) = x:(strain x (sieve xs))
sieve [] = []

strain x = filter (not . (`isMult`x))

isMult x n = x`mod`n == 0

isMultAny x [] = False
isMultAny x (n:ns) | x `isMult` n = True
                   | otherwise  = isMultAny x ns


floorsqrt x = floor (sqrt (fromRational(toRational (fromIntegral x))))

