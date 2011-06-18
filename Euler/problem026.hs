#!/usr/bin/runhaskell

import Data.List
import Data.Function (on)

main :: IO ()
main = print(soln 1000)

soln maxd = fst $ head $ sortBy (invcomp `on` snd) [(x,(cyclelength x)) | x<-[1..maxd]]
  where invcomp a b = compare b a
        cyclelength n = dec n 1 [] []
        dec n x digs rems | x`elem`rems = count x rems 0
                          | x == 0 = 0
                          | otherwise = dec n ((x*10)`mod`n) (((x*10)`div`n):digs) (x:rems)
        count _ [] acc = -1
        count n (x:xs) acc | n == x = acc+1
                           | otherwise = count n xs (acc+1)

