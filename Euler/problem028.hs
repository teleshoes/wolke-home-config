#!/usr/bin/runhaskell

main :: IO ()
main = print(soln 1001)

soln size = sum [(getDiags n) | n<-[1,3..size]]
 where getDiags 1 = 1
       getDiags n = 4*n^2 - 6*n + 6

--n^2 + (n^2 -n + 1) + (n^2 -2n + 2) (n^2 - 3n + 3)

--            43 44 45 46 47 48 49
--            42 21 22 23 24 25 26
--            41 20 07 08 09 10 27
--            40 19 06 01 02 11 28
--            39 18 05 04 03 12 29
--            38 17 16 15 14 13 30
--            37 36 35 34 33 32 31

