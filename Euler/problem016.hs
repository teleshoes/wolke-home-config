#!/usr/bin/runhaskell

import Data.Char

main :: IO ()
main = print(soln)

soln = sum $ map digitToInt $ show $ 2^1000

