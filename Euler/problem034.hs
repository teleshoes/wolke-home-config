#!/usr/bin/runhaskell

import Data.Char

main :: IO ()
main = print(soln)

soln = sum $ filter curious [3..upper 1]
 where upper n | (fac!!9) * n < ((10^n)-1) = (fac!!9)*n
               | otherwise = upper (n+1)
       curious n = n == (sum $ map (fac!!) $ digs n)
       digs n = map digitToInt (show n)
       fac = prodmap [1..] 1
       prodmap (x:xs) p = p:(prodmap xs (x*p))

