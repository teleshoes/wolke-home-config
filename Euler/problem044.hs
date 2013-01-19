#!/usr/bin/runhaskell

import Data.List
import Data.Function (on)
import Data.Maybe

main :: IO ()
main = print(soln)

--1) pentagonal numbers increase geometrically
--
--2) because (1),
--   the minimum difference between the nth pentagonal number
--   and any other pentagonal number is the difference between
--   the pentagonal number BEFORE it,
--   i.e.: the nth pentagonal number and the (n-1)th pentagonal number
--
--3) consider the difference between two adjacent pentagonal numbers, say,
--    the difference between the nth and the (n-1)th pentagonal numbers
--   because (1), as n increases, the difference increases.
--
--4) by (2)&(3),
--   given a positive integer W,
--   there exists a pentagonal number such that
--   all pentagonal numbers larger than it have a minimum difference
--   with another pentagonal that is greater than W
--
-- therefore, given D, one can find all pairs of
-- pentagonal numbers whose difference is <= D
-- by finding the pentagonal number whose minimum difference
-- is greater than D, and only examining pentagonal numbers
-- smaller than that ceiling
--
--
--Pk=7042750, k=2167
--Pj=1560090, j=1020
--D=Pd=5482660, d=1912
--S=Ps=8602840, s=2395
--
--Pm=5009929520597, m=1827554
--where Pm is the last pentagonal number whose minimum difference with
--another pentagonal number is less than or equal to D
--   (its min diff equals D)
--
--consider the next pentagonal number after Pm
--Pn=5009935003260, n=m+1=1827555
--
--Pn-Pm = 5482663 > D
--
--conclusion:
--  the difference between any pentagonal number above Pm=5009929520597
--  and ANY OTHER pentagonal number is strictly greater than D


soln = head $ solnsUnder Nothing pents [] []
 where
    solnsUnder ceil (k:ps@(p1:p2:_)) js solns 
     | not $ underCeil ceil (p2-p1) = solns
     | otherwise =
       let goodJs = takeWhile (\j-> underCeil ceil $ pdiff (k,j) ) js
           newsolns = sort $ solns ++ (map pdiff $ filter isSoln (map (\j->(k,j)) goodJs))
           newCeil = listToMaybe solns
           minDiff = p2-p1
       in solnsUnder newCeil ps (k:goodJs) (seq (newsolns) newsolns)

    underCeil (Just ceil) x = x<=ceil
    underCeil Nothing _ = True

    pdiff (k,j) = k-j
    isSoln (k,j) = isPent (k+j) && isPent (k-j)
    isPent pn = fromMaybe 0 (perfectSqrt (24*pn+1)) `mod` 6 == 5
    pents = map (\n->n*(3*n-1)`div`2) [1..]

    isqrt = floor . sqrt . fromInteger
    perfectSqrt n = let sqn = isqrt n
                    in if sqn*sqn == n then Just sqn else Nothing 

--more direct solution
soln' = filter isSoln (genallpairs (takeWhile (<pm) pents) [])
 where
    d=5482660
    pm=5009929520597
    genallpairs [] _ = []
    genallpairs (k:ps) psUnder = (pairsUnderD k psUnder) ++ (genallpairs ps (k:psUnder))
    pairsUnderD k js = (map (\j->(k,j)) (takeWhile (\q->k-q<= d) js))
    isSoln (k,j) = isPent (k+j) && isPent (k-j)
    isPent pn = fromMaybe 0 (perfectSqrt (24*pn+1)) `mod` 6 == 5
    pents = map (\n->n*(3*n-1)`div`2) [1..]

    isqrt = floor . sqrt . fromInteger
    perfectSqrt n = let sqn = isqrt n
                    in if sqn*sqn == n then Just sqn else Nothing



--ternary operator!
data Tern a = a :? a
infixl 1 ?
infixl 2 :?
test ? (x :? y) = if test then x else y

--1, 5, 12, 22, 35, 51, 70, 92, 117, 145

--Pk = k*(3*k - 1) `div` 2
--Pj = j*(3*j - 1) `div` 2
--Pd = Pk - Pj = d*(3*d - 1) `div` 2
--Ps = Pk + Pj = s*(3*s - 1) `div` 2

--k = (sqrt(24*Pk+1) + 1) `div` 6
--j = (sqrt(24*Pj+1) + 1) `div` 6
--s = (sqrt(24*Ps+1) + 1) `div` 6
--d = (sqrt(24*Pd+1) + 1) `div` 6


--lets assume k=j+2 for a tic
--Pk = (j+2)(3j+5)`div`2 = (3j^2+11j+10)/2
--Pj = (3j^2 - j)/2
--Pk-Pj = 6j+5
--Pk+Pj = 3j^2+5j+5
--sqrt(12*12j+11*11) `mod` 6 = 5



--lets assume theyre adjacent, i.e.: k=j+1
--Pk = (j+1)(3*(j + 1) - 1) `div` 2
--Pk = (j+1)(3j+2)`div`2
--Pk = (3j^2 + 5j + 2) `div` 2
--Pk - Pj = (3j^2 + 5j + 2 - (3j^2 - j)) `div` 2
--Pk - Pj = (6j + 2) `div` 2
--Pk - Pj = 3j+1
--Pk + Pj = 3j^2 + 2j + 1
--Pd = 3j+1
--Ps = 3j^2 + 2j + 1 
--sqrt(72j + 25) `mod` 6 == 5
--sqrt(72j^2+48j+25) `mod` 6 == 5
--sqrt(72j + 25) + 6i = sqrt(72j^2+48j+25)
--6i = sqrt(72j^2+48j+25) - sqrt(72j + 25)
-- is that possible? i dont think it is, so i dont think k=j+1



--what follows is bullshit
--Pk-Pj = Pd  and Pk+Pk = Ps
--lets assume that the smallest pair is adjacent.
--not true: this means i only have to check adjacent pairs smaller than it, since 
--if they are, then theyre cant be an adjacent pair with a smaller difference
--k+1=j

--pk = 7042750
--pj = 1560090

--d = pk - pj


        --countTrue = sum . map (\x->if x then 1 else 0)
        --oneanswer = head $ filtP allpairsUnsorted
        --allpairsUnsorted = [(k,j) | k<-pents, j<-takeWhile (<k) pents]

--        allpairs = pairs 0 (pents) []
--        pairs prev (p1:p2:ps) queue = let smalldiff = diff (p2,p1)
--                                          (now,qs) = span ((smalldiff >) . diff) queue
--                                          smallerPents = takeWhile (<prev) pents
--                                          pairsUnder = reverse $ filtP $ map (\x->(p1,x)) smallerPents
--                                          newQueue = (p2,p1) : (merge compDiff qs pairsUnder)
--                                      in now ++ (pairs p1 (p2:ps) (filtP newQueue))
--        
--        merge :: (Ord a)=>(a->a->Ordering)->[a]->[a]->[a]
--        merge cmp xs [] = xs
--        merge cmp [] ys = ys
--        merge cmp (x:xs) (y:ys) | cmp x y == LT = x:(merge cmp xs (y:ys))
--                                | otherwise     = y:(merge cmp (x:xs) ys)
--        compDiff = compare `on` diff

--        elemsorted es n = let xs = dropWhile (<n) es
--                          in xs /= [] && n == (head xs)
--        isPent' pn = elemsorted pents pn




--soln = take 1 [(x,y) | x<-pents, y<-[x..pents], isPent (x+y), isPent(abs(x-y))]--dropWhile (isNothing) $ take 100 $ map (try pents) pents--head $ dropWhile isNothing $ map (try pents) pents --take 1 $ filter (\(j,k)->isPent(j+k)) $ concatMap compose pents
-- wrote split instead of partition cause i couldnt remember the name
--        split f xs = splith f xs [] []
--        splith f [] good bad = (good,bad)
--        splith f (x:xs) good bad | f x = splith f xs (x:good) bad
--                                 | otherwise = splith f xs good (x:bad)

--        isPent n = elem n (takeWhile (<=n) pents)
--        pairs p = map (\x->(p,x)) $ reverse (takeWhile (<p) pents)
--        try (p1:p2:ps) x | isPent (x+p1) && isPent(abs$x-p1) = Just p1
--                         | x+p1<p2 = Nothing
--                         | otherwise = try (p2:ps) x
--        compose pn = let ps = limit pents pn
--                         diff = map (\p->(p,p-pn)) $ filter (\p->(elemsorted ps) (p-pn)) ps
--                     in diff
--        limit (p1:p2:ps) n | p2-p1>n = []
--                           | otherwise = p1:(limit (p2:ps) n)
--        test (p:ps) = let maybeComp=compose p
--                      in if (isJust maybeComp) then
--                             (if isPent (fst (fromJust maybeComp) - snd (fromJust maybeComp))
--                              then fromJust maybeComp
--                              else test ps)
--                         else test ps
--        composesingle pn = let ps = limit pents pn
--                               diff = dropWhile (not . (elemsorted ps) . (pn-)) ps
--                     in if length diff > 0 then Just (head diff, pn-(head diff)) else Nothing
--        decompose pn = let ps = takeWhile (<pn) pents
--                       in [(pj,pk) | pj<-ps, pk<-ps, (pj+ pk)==pn]--, (isPent (pj+pk))]
--        unpent p = head [n | n<-[1..p], 3*n*n-n==p*2]

