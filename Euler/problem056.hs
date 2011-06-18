#!/usr/bin/runhaskell

import Char (digitToInt)

main :: IO ()
main = print(soln)

soln = maximum $ map sumdigits [a^b | a<-[1..100], b<-[1..100]]
 where
   sumdigits x = sum $ map digitToInt $ show x

