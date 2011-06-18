#!/usr/bin/runhaskell

import Data.List
main :: IO ()
main = print(countzeros firstofthemonth 0)

firstofthemonth = [dow 1 m y | y<-[1901..2000], m<-[1..12]]

countzeros [] cnt = cnt
countzeros (0:xs) cnt = countzeros xs (cnt+1)
countzeros (_:xs) cnt = countzeros xs cnt

dow :: Int->Int->Int->Int
dow d m y = (1 + d + (dayssince m y 0))`mod`7--(y*365 + ((y-1900) `div` 4)) + (sum ())

dayssince :: Int->Int->Int->Int
dayssince m y _ | m<=0 || m>12 || y<1901 = error "wants more love"
dayssince 1 1901 acc = acc
dayssince 1 y acc = dayssince 1 (y-1) (acc+(daysinyear (y-1)))
dayssince m y acc = dayssince (m-1) y (acc+(daysinmonth (m-1) y))

daysinyear y | isLeapYear y = 366
             | otherwise = 365

daysinmonth m y | m==1 || m==3 || m==5 || m==7 || m==8 || m==10 || m==12 = 31
                | m==9 || m==4 || m==6 || m==11 = 30
                | (m==2 && (isLeapYear y)) = 29
                | otherwise = 28

isLeapYear y = (y`mod`4==0) && ((not (y`mod`100==0)) || y`mod`400==0)

