#!/usr/bin/runhaskell

import Data.List

main :: IO ()
main = print(soln 200)

soln pence = length [(p1, p2, p5, p10, p20, p50, p100, p200) |
  p1  <-0:[1..(200`div`1)],
  (pence >= p1*1),
  p2  <-0:[1..(200`div`2)],
  (pence >= p1*1 + p2*2),
  p5  <-0:[1..(200`div`5)],
  (pence >= p1*1 + p2*2 + p5*5),
  p10 <-0:[1..(200`div`10)],
  (pence >= p1*1 + p2*2 + p5*5 + p10*10),
  p20 <-0:[1..(200`div`20)],
  (pence >= p1*1 + p2*2 + p5*5 + p10*10 + p20*20),
  p50 <-0:[1..(200`div`50)],
  (pence >= p1*1 + p2*2 + p5*5 + p10*10 + p20*20 + p50*50),
  p100<-0:[1..(200`div`100)],
  (pence >= p1*1 + p2*2 + p5*5 + p10*10 + p20*20 + p50*50 + p100*100),
  p200<-0:[1..(200`div`200)],
  (pence == p1*1 + p2*2 + p5*5 + p10*10 + p20*20 + p50*50 + p100*100 + p200*200)
 ]


