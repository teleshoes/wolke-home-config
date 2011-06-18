#!/usr/bin/runhaskell

import Data.List
import Data.Set (showTree, fromList, size, member)

main :: IO ()
main = print(soln)

--all integers greater than 28123 can
--be written as the sum of two abundant numbers

soln = sum(filter (not . anyd) [1..28123])
anyd x = anydiff x abundantnums
anydiff _ [] = False
anydiff x (a:as) | x<a = False
                 | (member (x-a) setofabundantnums) = True
                 | otherwise = anydiff x as

setofabundantnums = fromList abundantnums

abundantnums = filter isAbundant [1..28123]

isAbundant x = sumpropfax x > x
sumpropfax x = let halfFax = filter (\d->x`mod`d==0) [1..(floor$sqrt$fromInteger x)]
               in (sum $ nub $ halfFax ++ map (x`div`) halfFax) - x

--isAbundant x = (sum (properfactors x)) > x
--properfactors x = filter ((==) 0 . (mod x)) [1..(x-1)]


--perms :: Int -> [a] -> [[a]]
--perms _ [] = []
--perms 1 xs = [[x] | x<-xs]
--perms n (x:xs) = [e:p | (e,ps) <- (permsh [] x xs), p<-(perms (n-1) ps)]

--permsh :: [a] -> a -> [a] -> [(a,[a])]
--permsh xs y [] = [(y,xs)]
--permsh xs y (z:zs) = (y,xs++(z:zs)):(permsh (y:xs) z zs)

