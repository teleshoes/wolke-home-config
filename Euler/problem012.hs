#!/usr/bin/runhaskell

import Data.List

main :: IO ()
main = print(unjust (find (\x -> factorCount x > 500) triNums))

unjust (Just x) = x
unjust Nothing = error "Nothing"

triNums = triNum [1..] 0
triNum (x:xs) sum = (x+sum) : (triNum xs (x+sum))

factorCount :: Int -> Int
factorCount x = factorCounth x [1..(floorsqrt x)] 0
factorCounth _ [] cnt = cnt*2
factorCounth x (n:ns) cnt | isMultiple n x = factorCounth x ns (cnt+1)
                          | otherwise =      factorCounth x ns cnt

floorsqrt :: Int -> Int
floorsqrt x = floor (sqrt (fromRational(toRational (fromIntegral x))))

isMultiple :: Int -> Int -> Bool
isMultiple n x = (x `mod` n) == 0

