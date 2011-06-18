#!/usr/bin/runhaskell

import Data.Maybe
import Data.List
import Data.Function

main :: IO ()
main = print(soln 1000)

--p = a+b+c
--a^2 + b^2 == c^2

--b=p-a-sqrt(a^2+b^2)
--b-p+a=-sqrt(a^2+b^2)
--(b-p+a)^2 = a^2+b^2
--b^2-bp+ab-bp+p^2-pa+ab-ap+a^2=a^2+b^2
--a^2+b^2+2ab+p^2-2bp-2ap=a^2+b^2
--2ab-2bp=2ap-p^2
--b*(2a-2p)=(2ap-p^2)
--b=(2ap-p^2)/(2a-2p)
--b=p(p-2a)/2(p-a)

soln n = fst $ maximumBy (compare `on` snd) $ map (\p->(p,tris p)) [1..n]
 where tris p = length $ catMaybes $ map (maybeB p) [1..(p-1)]
       maybeB p a = let top = p * (p - 2*a)
                        bot = 2 * (p - a)
                        b = top`div`bot
                    in if top`mod`bot == 0 && 0<b&&b<a then Just b else Nothing

