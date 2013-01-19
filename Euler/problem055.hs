#!/usr/bin/runhaskell

import Data.Char (intToDigit, digitToInt)

main :: IO ()
main = print(soln 10000)

soln n = length $ filter (isLychrel 50) [1..(n-1)]
 where
   isLychrel limit x = and $ map (not.isPal) $ take limit (lychrations x)
   lychrations x = drop 1 $ iterate lychrate (digits x)
   lychrate ds = digits $ (undigits ds) + (undigits $ reverse ds)

   isPal digs = digs == (reverse digs)

   digits x = map digitToInt $ show x
   undigits xs = read $ map intToDigit xs :: Integer

