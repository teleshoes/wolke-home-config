#!/usr/bin/runhaskell

import Data.List
import Data.Char(digitToInt)
import Data.Function

main :: IO ()
main = do
 file <- readFile "problem054.txt"
 print (soln $ map init $ lines file)

soln games = length $ filter ((1==).winner) games
 where
  breakUp _ [] = []
  breakUp n xs = take n xs:breakUp n (drop n xs)
  removeAll n xs | not $ elem n xs = xs
                 | otherwise = removeAll n (xs\\[n])
  winner game = let cards = breakUp 2 $ removeAll ' ' game
                    p1 = take 5 cards
                    p2 = take 5 $ drop 5 cards
                in p1 `vs` p2
  suit card = case card !! 1 of
                'C' -> 0
                'D' -> 1
                'H' -> 2
                'S' -> 3
  value card = case card !! 0 of 
                'T' -> 10
                'J' -> 11
                'Q' -> 12
                'K' -> 13
                'A' -> 14
                _   -> digitToInt (card !! 0)

  cmp a b | a>b = 1
          | b>a = 2
          | otherwise = error "shouldnt happen in this game"
  
  high hand = True
  highTie h1 h2 = cmp (reverse$sort(map value h1)) (reverse$sort(map value h2))

  pair hand = let vals = sort(map value hand)
              in 2 `elem` (map length (group vals))
  pairTie h1 h2 = let vals1 = sort(map value h1)
                      vals2 = sort(map value h2)
                      rank1 = head $ head $ filter ((==2).length) (group vals1)
                      rank2 = head $ head $ filter ((==2).length) (group vals2)
                      kick1 = reverse $ vals1 \\ [rank1,rank1]
                      kick2 = reverse $ vals2 \\ [rank2,rank2]
                  in if rank1 /= rank2 then cmp rank1 rank2 else cmp kick1 kick2

  twopair hand = let vals = sort(map value hand)
                 in 2 == (length $ filter (==2) (map length (group vals)))
  twopairTie h1 h2 = let vals1 = sort(map value h1)
                         vals2 = sort(map value h2)
                         rankH1 = head $ last $ filter ((==2).length) (group vals1)
                         rankH2 = head $ last $ filter ((==2).length) (group vals2)
                         rankL1 = head $ head $ filter ((==2).length) (group vals1)
                         rankL2 = head $ head $ filter ((==2).length) (group vals2)
                         kick1 = reverse $ vals1 \\ [rankH1,rankH1,rankL1,rankL1]
                         kick2 = reverse $ vals2 \\ [rankH2,rankH2,rankL2,rankL2]
                     in if rankH1 /= rankH2 then cmp rankH1 rankH2 else
                           if rankL1 /= rankL2 then cmp rankL1 rankL2 else cmp kick1 kick2

  threekind hand = let vals = sort(map value hand)
                   in 3 `elem` (map length (group vals))
  threekindTie h1 h2 = let vals1 = sort(map value h1)
                           vals2 = sort(map value h2)
                           rank1 = head $ head $ filter ((==3).length) (group vals1)
                           rank2 = head $ head $ filter ((==3).length) (group vals2)
                           kick1 = reverse $ vals1 \\ [rank1,rank1,rank1]
                           kick2 = reverse $ vals2 \\ [rank2,rank2,rank2]
                       in if rank1 /= rank2 then cmp rank1 rank2 else cmp kick1 kick2

  straight hand = let vals = sort(map value hand)
                  in vals == [2,3,4,5,14] || vals == [(head vals)..(last vals)]
  straightTie h1 h2 = highTie h1 h2
  
  flush hand = let suits = map suit hand
                   s = head suits
               in all (==s) suits
  flushTie h1 h2 = highTie h1 h2

  fullhouse hand = threekind hand && pair hand
  fullhouseTie h1 h2 = let vals1 = sort(map value h1)
                           vals2 = sort(map value h2)
                           rank31 = head $ head $ filter ((==3).length) (group vals1)
                           rank32 = head $ head $ filter ((==3).length) (group vals2)
                           rank21 = head $ head $ filter ((==2).length) (group vals1)
                           rank22 = head $ head $ filter ((==2).length) (group vals2)
                       in if rank31 /= rank32 then cmp rank31 rank32 else cmp rank21 rank22

  fourkind hand = let vals = sort(map value hand)
                  in 4 `elem` (map length (group vals))
  fourkindTie h1 h2 = let vals1 = sort(map value h1)
                          vals2 = sort(map value h2)
                          rank1 = head $ head $ filter ((==4).length) (group vals1)
                          rank2 = head $ head $ filter ((==4).length) (group vals2)
                      in cmp rank1 rank2
                       
  straightflush hand = flush hand && straight hand
  straightflushTie h1 h2 = highTie h1 h2
  
  handle h1 h2 (handTest, handTie)
   | handTest h1 && handTest h2 = handTie h1 h2
   | handTest h1 = 1
   | handTest h2 = 2
   | otherwise = 0

  p1 `vs` p2 = let checks = [(straightflush,straightflushTie),
                             (fourkind,fourkindTie),
                             (fullhouse,fullhouseTie),
                             (flush,flushTie),
                             (straight,straightTie),
                             (threekind,threekindTie),
                             (twopair,twopairTie),
                             (pair,pairTie),
                             (high,highTie)]
               in head $ dropWhile (==0) $ map (handle p1 p2) checks

