#!/usr/bin/runhaskell

import Data.List (sort)
import Data.Char (digitToInt)

main :: IO ()
main = print(soln 5)

soln n = cubePerms n 0 [] cubes
 where
   cubePerms n pow10 preCs (c:cs) =
     let c10 = digs c
         okCs = if c10 > pow10 then [] else preCs
         perms = filter (\p->(isPerm c p)) okCs
         cnt = 1 + length perms
         in if cnt == n
            then minimum perms
            else cubePerms n c10 (c:okCs) cs

   cubes = map (^3) [1..]
   
   digs c = length $ takeWhile (>0) $ iterate (`div`10) c

   digits x = map digitToInt $ show x

   isPerm x y = let digX = digits x
                    digY = digits y
                    sumEq = sum digX == sum digY
                    eq = sort digX == sort digY
                    in sumEq && eq

