#!/usr/bin/runhaskell

import Char

main :: IO ()
main = print(soln)

soln = sum $ map digitToInt $ show $ fac 100
fac x = fach x 1
fach x prod | x==1 = prod | otherwise = (fach (x-1) prod*x)
