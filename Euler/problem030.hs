#!/usr/bin/runhaskell

import Data.Char

main :: IO ()
main = print(soln 5)

soln exp = sum $ map fst $ filter same (map sumpair [2..(upperlimit exp 1)])
 where digs x = map digitToInt $ show x
       --an upper limit is just 10^(exp+1)
       upperlimit n d | (9^n*d) <= (10^d)-1 = (10^d)-1
                      | otherwise           = upperlimit n (d+1)
       same (a,b) = a == b
       sumpair x = (x, sumpows exp 0 (digs x))
       sumpows _ acc [] = acc
       sumpows n acc (x:xs) = sumpows n (acc+x^n) xs

