#!/usr/bin/runhaskell

import Char
import Data.List

main :: IO ()
main = print(soln)

--987654321

-- p is the concatenation of the product of x and each of [1..n],
--   n>1, p is 1 to 9 pandigital

--since n>1, p must be the concatenation of at least two numbers >= x
--that means that at least one of these numbers has only 4 digits,
-- and therefore

--x <= 4 digits

soln = maximum $ map pandig [(x,[1..n]) | x<-[1..10^4-1], n<-[2..9], (ds x)*n<=9]
 where pandig (x,ps) = let dlist = concatMap (digs.(x*)) ps
                       in if isPand dlist then undigs dlist else 0
       isPand xs = length xs == 9 && and [elem d xs | d<-[1..9]]
       ds = (+) 1 . floor . logBase 10 . fromInteger
       digs n = map digitToInt (show n)
       undigs xs = toInteger $ read $ map intToDigit xs

