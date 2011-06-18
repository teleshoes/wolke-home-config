#!/usr/bin/runhaskell

main :: IO ()
main = print(sumPrimesUnder 2000000)

sumPrimesUnder n = sieveSum n [2..n] 0
 where sieveSum lim (x:xs) acc
           | x^2<lim   = sieveSum lim (strain x xs) (acc+x)
           | otherwise = acc + (sum (x:xs))
       strain x = filter (not . isMultiple x)
       isMultiple n x = (x `mod` n) == 0

