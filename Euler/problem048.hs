#!/usr/bin/runhaskell

main :: IO ()
main = print(soln 1000)

soln n = (sum [x^x | x<-[1..n]]) `mod` (10^10)

