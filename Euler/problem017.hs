#!/usr/bin/runhaskell

import Char

main :: IO ()
main = print(soln)

soln = sum . map length $ map word [1..1000]

word x | x==0  = ""
       | x==1  = "one"
       | x==2  = "two"
       | x==3  = "three"
       | x==4  = "four"
       | x==5  = "five"
       | x==6  = "six"
       | x==7  = "seven"
       | x==8  = "eight"
       | x==9  = "nine"
       | x==10 = "ten"
       | x==11 = "eleven"
       | x==12 = "twelve"
       | x==13 = "thirteen"
       | x==14 = "fourteen"
       | x==15 = "fifteen"
       | x==16 = "sixteen"
       | x==17 = "seventeen"
       | x==18 = "eighteen"
       | x==19 = "nineteen"
       | x==20 = "twenty"
       | x==30 = "thirty"
       | x==40 = "forty"
       | x==50 = "fifty"
       | x==60 = "sixty"
       | x==70 = "seventy"
       | x==80 = "eighty"
       | x==90 = "ninety"
       | x`mod`1000 == 0 = word(x`div`1000) ++ "thousand"
       | x`mod`100 == 0 = word(x`div`100) ++ "hundred"
       | x<100 = word (x`div`10*10) ++ word (x`mod`10)
       | x<1000 = word (x`div`100*100) ++ "and" ++ word (x`mod`100)
       | x<10000 = word (x`div`1000*1000) ++ word (x`mod`1000)

