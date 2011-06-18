#!/usr/bin/runhaskell

import Char

main :: IO ()
main = print(soln 1000000)

soln n = sum $ filter paldecbin [1..n]
  where paldecbin x = (pal $ digits x 10) && (pal $ digits x 2)
        pal xs = xs == (reverse xs)
        digits x base | x<base = [x]
                      | otherwise = x`mod`base:(digits (x`div`base) base)

