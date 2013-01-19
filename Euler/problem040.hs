#!/usr/bin/runhaskell

import Data.Char

main :: IO ()
main = print(soln [1,10,100,1000,10000,100000,1000000])

soln ns = product $ map d ns
 where d n = frac !! (n-1)
       frac = num 1
       num x = digs x ++ num (x+1)
       digs n = map digitToInt $ show n

