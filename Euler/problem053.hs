#!/usr/bin/runhaskell

main :: IO ()
main = print(soln)

soln = length $ filter (>10^6) [n`c`r | n<-[1..100], r<-[0..n]]

c :: Integer -> Integer -> Integer
n `c` k = choose n (min k (n-k)) 1 1
 where choose n k np kp
        | k==0 = np `div` kp
        | otherwise = choose (n-1) (k-1) (n*np) (k*kp)

