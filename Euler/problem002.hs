#!/usr/bin/runhaskell

main :: IO ()
main = print(sumlist smallevenfibbylist)

fibbylist = map' fib [1..]

isEven :: Int -> Bool
isEven x = x >= 0 && x `mod` 2 == 0


evenfibbylist = filter isEven fibbylist

smallevenfibbylist = takeWhile (<=4000000) evenfibbylist

sumlist :: [Int] -> Int
sumlist [] = 0
sumlist (x:xs) = x + sumlist xs

fib :: Int -> Int
fib 1 = 1
fib 2 = 2
fib n = fib (n-1) + fib (n-2)

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) | f x = x : filter' f xs
filter' f (x:xs) = filter' f xs

map' :: (a -> a) -> [a] -> [a]
map' _ [] = []
map' f (x:xs) = f x : map f xs

