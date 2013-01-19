#!/usr/bin/runhaskell

import Data.List
import Data.Char

main :: IO ()
main = do
 wordFile <- readFile "problem042.txt"
 print (soln (read ("["++wordFile++"]")))

soln words = length $ filter isTrinum words
  where isTrinum word = isTri trinums $ sum $ map charval word
        charval c = (ord c) - 64
        isTri (t:ts) x | x==t = True
                       | x<t = False
                       | otherwise = isTri ts x
        trinums = map (\n->n*(n+1)`div`2) [1..]

