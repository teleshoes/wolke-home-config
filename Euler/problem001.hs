#!/usr/bin/runhaskell

main :: IO ()
main = print(sumlist (filter' (\x -> (isMultiple 3 x) || (isMultiple 5 x)) [1..1000-1]))

sumlist :: [Int] -> Int
sumlist xs = sumlisth xs 0
sumlisth [] sum = sum
sumlisth (x:xs) sum = sumlisth xs x+sum

isMultiple :: Int -> Int -> Bool
isMultiple n x
  | n <= 0 = error "positive integers only"
  | x < 0  = error "non-negative integers only"
isMultiple n x = (x `mod` n) == 0

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) | f x = x : filter' f xs
filter' f (x:xs) = filter' f xs


