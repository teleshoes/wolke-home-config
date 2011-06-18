#!/usr/bin/runhaskell

main :: IO ()
main = print(ssdif [1..100])

ssdif :: [Int] -> Int
ssdif xs = squaresums xs - sumsquares xs

sumlist :: [Int] -> Int
sumlist xs = sumlisth xs 0
sumlisth [] sum = sum
sumlisth (x:xs) sum = sumlisth xs x+sum

map' :: (a -> a) -> [a] -> [a]
map' _ [] = []
map' f (x:xs) = f x : map f xs

square :: Int -> Int
square x = x*x

sumsquares :: [Int] -> Int
sumsquares xs = sumlist (map' square xs)

squaresums :: [Int] -> Int
squaresums xs = square (sumlist xs)

