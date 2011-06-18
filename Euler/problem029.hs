#!/usr/bin/runhaskell

import Data.Set (empty, insert, size)

main :: IO ()
main = print(soln 100)

soln range = distinct [(a,b) | a<-[2..range], b<-[2..range]] empty
 where distinct [] set = size set
       distinct ((a,b):xs) set = distinct xs (insert (a^b) set)

