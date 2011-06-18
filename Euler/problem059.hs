#!/usr/bin/runhaskell

import Data.Function
import Data.List
import Data.Char (chr,ord,toLower)
import Data.Bits

main :: IO ()
main = do
 wordFile <- readFile "problem059.txt"
 print (soln (read ("["++wordFile++"]") :: [Int]))

soln asciis = sum $ untext $ decrypt asciis
 where
   decrypt asciis =
     let pwords = [[a,b,c] | a<-lc, b<-lc, c<-lc]
         decodes = map (decode asciis) pwords
         likely = filter (containsIC "the") decodes
     in minimumBy (compare `on` errSum) $ (seq likely likely)

   decode asciis pw = let pass = take (length asciis) (cycle pw)
                      in text $ zipWith xor asciis pass

   text = map chr
   untext = map ord

   count _ [] acc = acc
   count ch (s:str) acc | ch == s = (count ch str (acc+1))
                        | otherwise = count ch str acc

   expected c = case c of {
    'a'->8.167;'b'->1.492;'c'->2.782;'d'->4.253;'e'->12.702;'f'->2.228;'g'->2.015;
    'h'->6.094;'i'->6.966;'j'->0.153;'k'->0.772;'l'->4.025;'m'->2.406;'n'->6.749;
    'o'->7.507;'p'->1.929;'q'->0.095;'r'->5.987;'s'->6.327;'t'->9.056;'u'->2.758;
    'v'->0.978;'w'->2.360;'x'->0.150;'y'->1.974;'z'->0.074}

   importantLetters = "etaoins"

   errSum str = let allF = sum $ map (\ch->(count ch str 0)) (text lc)
                    freq c = 100 * (count c str 0) / allF
                    error c = abs $ (freq c) - (expected c)
                in product $ map (\ch->error ch) importantLetters

   --this works just as well min(errSum) and is fast fast fast
   isEnglish text = containsIC "the " text && containsIC "of " text

   containsIC sub str = contains (map toLower sub) (map toLower str)

   contains _ [] = False
   contains sub str | sub == (take (length sub) str) = True
                    | otherwise = contains sub (tail str)

   lc = [97..122]

