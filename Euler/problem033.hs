#!/usr/bin/runhaskell

import Data.Char

main :: IO ()
main = print(soln)

soln = snd $ reduce $ mult (1,1) $ filter tricky [(x,y) | x<-[10..99], y<-[(x+1)..99]]
 where tricky f = same f (silly f)
       silly (x,y) = sillify (digs x, digs y) [1..9]
       sillify _ [] = (-1,-1)
       sillify ((x1:x2:[]),(y1:y2:[])) (d:ds) | x1 == d && y1 == d = (x2,y2)
                                              | x1 == d && y2 == d = (x2,y1)
                                              | x2 == d && y1 == d = (x1,y2)
                                              | x2 == d && y2 == d = (x1,y1)
                                              | otherwise = sillify ((x1:x2:[]),(y1:y2:[])) ds
       digs n = map digitToInt (show n)
       same (x1,y1) (x2,y2) = (x1*y2)==(x2*y1)
       mult (num,den) [] = (num,den)
       mult (num,den) ((x,y):xs) = mult (num*x,den*y) xs
       reduce (x,y) = let n = gcd x y
                      in if n==1 then (x,y) else reduce (x`div`n, y`div`n)


--trivial (x,y) = x`mod`10==0 && y`mod`10==0

