#!/usr/bin/runhaskell

import Char (intToDigit, digitToInt)
import Data.List

main :: IO ()
main = print(soln)

--Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.
--  you dont need x to have the same digits, hence drop 1, grr...

soln = undigits $ head $ head $ filter (sameAll.(drop 1)) $ map (digMults 6) [1..]
 where
   digMults n x = map digits $ scanl1 (+) (replicate n x)
   sameAll (xs:ys:xss) = same xs ys && sameAll (ys:xss)
   sameAll _ = True
   same xs ys = (xs\\ys) == [] && (ys\\xs) == []
   digits x = map digitToInt $ show x :: [Int]
   undigits xs = read $ map intToDigit xs :: Integer

