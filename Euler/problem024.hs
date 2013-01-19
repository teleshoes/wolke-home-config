#!/usr/bin/runhaskell

import Data.Char

main :: IO ()
main = print(soln 1000000)

soln n = num $ sortedPandigs !! (n-1)
 where
   num xs = read $ map intToDigit xs :: Integer
   sortedPandigs = greedyperms [0..9]
   greedyperms [] = [[]]
   greedyperms [x] = [[x]]
   greedyperms ns = concatMap (genPerms ns) ns
   genPerms ns n = map (n:) (greedyperms $ ns`sans`n)
   ns `sans` n = filter (/=n) ns

