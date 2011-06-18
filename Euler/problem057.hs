#!/usr/bin/runhaskell

import Char (digitToInt)

main :: IO ()
main = print(soln 1000)

soln n = length $ filter (\(a,b)->a>b) $ take n $ map diglen sqrt2
 where
   sqrt2 = iterate next (3,2)
   next (num,den) = ((num+den+den),(num+den))

   diglen (a,b) = (length $ digits a, length $ digits b)
   digits x = map digitToInt $ show x

