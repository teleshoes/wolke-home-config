#!/usr/bin/runhaskell

import Data.List

main :: IO ()
main = print(soln 10000)

soln max = sum $ map snd $ filter amicable sums
  where amicable (i,x) = not (i==x) && x>0 && x<=max && i==(snd (sums!!(x-1)))
        sums = map (\x->(x,(sum $ properfactors x))) [1..max]
        properfactors x = filter ((==) 0 . (mod x)) [1..(x-1)]

--stuff
aproperfactors x = 1:((squares x)++ fax ++ (map (div x) fax))
  where fax=(filter (isDivisibleBy x) [2..((floorsqrt x) - 1)])
        squares a | (floorsqrt a)*(floorsqrt a) == a = [(floorsqrt a)]
        squares a | otherwise = []
        isDivisibleBy q n = q`mod`n==0
        floorsqrt y = floor (sqrt (fromRational(toRational (fromIntegral y))))
