#!/usr/bin/runhaskell

import Data.Char
import Data.List

main :: IO ()
main = print(soln)

soln = sum [num[d1,d2,d3,d4,d5,d6,d7,d8,d9,da] |
  d1<-[0..9] \\ [],
  d2<-[0..9] \\ [d1],
  d3<-[0..9] \\ [d1,d2],
  d4<-[0..9] \\ [d1,d2,d3],                   divs (num [d2,d3,d4]) 2,
  d5<-[0..9] \\ [d1,d2,d3,d4],                divs (num [d3,d4,d5]) 3,
  d6<-[0..9] \\ [d1,d2,d3,d4,d5],             divs (num [d4,d5,d6]) 5,
  d7<-[0..9] \\ [d1,d2,d3,d4,d5,d6],          divs (num [d5,d6,d7]) 7,
  d8<-[0..9] \\ [d1,d2,d3,d4,d5,d6,d7],       divs (num [d6,d7,d8]) 11,
  d9<-[0..9] \\ [d1,d2,d3,d4,d5,d6,d7,d8],    divs (num [d7,d8,d9]) 13,
  da<-[0..9] \\ [d1,d2,d3,d4,d5,d6,d7,d8,d9], divs (num [d8,d9,da]) 17
 ]
 where num ds = toInteger $ read $ map intToDigit ds
       divs x y = x`mod`y==0





soln' = length $ filter hasProp $ permutations "0123456789"
 where sub num i = toInteger $ read $ take 3 $ drop i num
       hasProp num = sub num 2 `mod` 2 == 0 &&
                     sub num 3 `mod` 3 == 0 &&
                     sub num 4 `mod` 5 == 0 &&
                     sub num 5 `mod` 7 == 0 &&
                     sub num 6 `mod` 11 == 0 &&
                     sub num 7 `mod` 13 == 0 &&
                     sub num 8 `mod` 17 == 0
