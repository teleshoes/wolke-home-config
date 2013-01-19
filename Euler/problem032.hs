#!/usr/bin/runhaskell

import Data.Char
import Data.List

--multiplicand and multiplier have to be 4 digits or less,
--since the floor of the digits of the product is the
--max(digits of multiplcand, digits of multiplier)

--ds x = 1,2,3,4
--ds y = 4,3,2,1

main :: IO ()
main = print(soln)

soln = sum $ nub $ map fst $ filter pandig prods
 where prods = [(x*y,(x,y)) | x<-[1..(10^4-1)], y<-[1..(10^(5-(ds x))-1)]]
       ds = (+) 1 . floor . logBase 10 . fromInteger
       pandig (x,(y,z)) = let dlist = (digs x ++ digs y ++ digs z)
                          in length dlist == 9 && and [elem digit dlist | digit<-[1..9]]
       digs n = map digitToInt (show n)

--ds x = dsh x 1
--dsh x d | x<10 = d
--        | otherwise = dsh (x`div`10) (d+1)

