#!/usr/bin/runhaskell

import Data.List

main :: IO ()
main = print(soln 6)

--if x is the nth s-gonal number,
--n = (sqrt((8s-16)x+(s-4)^2) + s - 4) / (2*s - 4)
-- therefore, if n is an integer, x is s-gonal

soln n = sum $ head $ cycles n
 where
   polys4d s = filter (isPoly s) [1000..9999]
   isPoly s x =
     let qty = (8*s-16)*x + (s-4)^2
         top = (floorsqrt qty) + s - 4
         btm = 2*s-4
         in perfectSq qty && top`mod`btm==0

   perfectSq x = x == (floorsqrt x)^2
   floorsqrt = floor.sqrt.fromInteger

   cycles n =
     let polys = map polys4d [3..(3+n-1)]
         in concatMap (chains []) $ bracelets polys

   bracelets [] = []
   bracelets (x:xs) = map (x:) $ permutations xs

   chains [] [] = []
   chains [] (ps:pss) = concatMap (\p-> chains [p] pss) ps
   chains chain (ps:pss) =
     let linkablePs = filter (\p->(head chain)`links`p) ps
         in concatMap (\p-> chains (p:chain) pss) linkablePs
   chains chain [] | (head chain)`links`(last chain) = [chain]
                   | otherwise = []

   x`links`y = x`mod`100 == y`div`100

