#!/usr/bin/runhaskell

import Data.Set (empty, insert, member)

main :: IO ()
main = print(soln 10000)

soln n = length $ filter ((==1).(`mod`2)) (map period [2..n])
 where

   period n | isPerfectSq n = 0
            | otherwise = length $ sequence n

   sequence n = let flsq = floorsqrt n
                    in getas ((n,1,-flsq),1) empty

   getas (top@(rad,coef,int),btm) found =
     let nextfrac = (times (rad,0,btm) (conj top), conjtimes top)
         (a,nextTop,nextBtm) = yank nextfrac
         (nextTopRed, nextBtmRed) = reduce (nextTop,nextBtm)
         in if (nextTopRed,nextBtmRed)`member`found
            then []
            else a:(getas (nextTopRed,nextBtmRed) (insert (nextTopRed,nextBtmRed) found))

   reduce (top@(rad,coef,int),btm) =
     let den = gcd (gcd coef int) btm
         in ((rad,coef`div`den,int`div`den),btm`div`den)

   yank (top@(rad,coef,int),btm) =
     let sq = sqrt $ fromInteger rad
         flr = (floor (sq*(fromInteger coef))) + int
         a = flr`div`btm
         in (a,(rad,coef,int-btm*a),btm)

   conj (rad,coef,int) = (rad, coef, 0-int)

   conjtimes num = let (_,_,res) = times num (conj num)
                       in res

   times (rad1,coef1,int1) (rad2,coef2,int2)
     | rad1 /= rad2 = error "not same sqrt"
     | otherwise = (rad1, (coef1*int2 + coef2*int1), int1*int2 + (rad1*coef1*coef2))

   floorsqrt x = floor $ sqrt $ fromInteger x

   isPerfectSq x =
     let fsq = floor $ sqrt $ fromInteger x
         in fsq^2 == x || (fsq+1)^2 == x

