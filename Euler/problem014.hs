#!/usr/bin/runhaskell

import Data.List
import Data.Ord
import qualified Data.Map as Map

main :: IO ()
main = print(soln)

type Mop = Map.Map Integer Integer
type MopPair = (Integer,Integer)

soln = fst $ biggestchain [1..1000000] (Map.empty,(-1,-1))
 where biggestchain :: [Integer] -> (Mop, MopPair) -> MopPair
       biggestchain [] (map,max) = max
       biggestchain (x:xs) (map,max) = biggestchain xs (build x (map,max))
       build :: Integer -> (Mop, MopPair) -> (Mop, MopPair)
       build x (map,(i,chlen)) =
         case (Map.lookup x map) of
           Just val -> (map,(i,chlen))
           Nothing -> let chainlength = (len x 0 map)
                      in let newmap = (Map.insert x chainlength map)
                             newmax = (if chainlength > chlen then (x,chainlength) else (i,chlen))
                         in (newmap, newmax)
       len 1 cur map = cur+1
       len x cur map = case (Map.lookup x map) of
                              Just val -> cur+val
                              Nothing  -> len (next x) (cur+1) map
       next x | x`mod`2==0 = x`div`2
              | otherwise  = 3*x+1



brute = head (sortBy (revcomp snd) (chains [1..(1000000-1)]))
 where revcomp f a b = comparing f b a
       chains xs = map convert xs
       convert x = (x, (con x 0))
       con 1 len = len
       con x len | x`mod`2==0    = con (x `div` 2) (len+1)
                 | otherwise = con ((3*x) + 1) (len+1)
       

--main = print(isHeadOf (1) [(4,2,3), (1,2,3)])

--main = print(chains nums)
--main = print(head (sortBy sortchains (chains nums)))

solna = head (sortBy sortchains (chains nums))

nums = [999999,999998..1]


sortchains :: (Int, Int, [Int]) -> (Int, Int, [Int]) -> Ordering
sortchains (_,len1,_) (_,len2,_) = compare len2 len1

chains :: [Int] -> [(Int, Int, [Int])]
chains xs = cha xs []
cha [] res = res
cha (x:xs) res = cha xs ((x,len,ch):res)
                 where (ch, len) = makeChain x res

first :: (a,b,c) -> a
first (a,b,c) = a

isHeadOf :: Int -> [(Int,b,c)] -> Bool
isHeadOf _ [] = False
isHeadOf n (x:xs) = (n == (first x)) || (isHeadOf n xs)


getHeadOf :: Int -> [(Int,b,c)] -> (Int,b,c)
getHeadOf _ [] = error "couldnt get head of"
getHeadOf n (x:xs) | (n == (first x)) = x
                   | otherwise = getHeadOf n xs

makeChain :: Int -> [(Int, Int, [Int])] -> ([Int], Int)
makeChain x res = makeCh x [] 0 res
makeCh 1 xs len res = ((1:xs), len+1)
makeCh x xs len res | isHeadOf x res = (xs++prevchain, len+prevlen)
                                   where (prevx,prevlen,prevchain) = getHeadOf x res
makeCh x xs len res | isEven x  = makeCh (x `div` 2) (x:xs) (len+1) res
                    | otherwise = makeCh ((x*3) + 1) (x:xs) (len+1) res


isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

