#!/usr/bin/runhaskell

main :: IO ()
main = print(soln 1000)

soln n = fst $ head $ dropWhile (\(i,x) -> x < ndigits) fiblist
  where ndigits = 10^(n-1)


fiblist = fib 1 1 1
fib i x y = (i,x):(fib (i+1) (y) (x+y))

