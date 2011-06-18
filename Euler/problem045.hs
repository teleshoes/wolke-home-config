#!/usr/bin/runhaskell

import Data.List
import Data.Function (on)
import Maybe

main :: IO ()
main = print(soln)

soln = head $ dropWhile(<=40755) $ tris `intrsex` pens `intrsex` hexs
  where
    (x:xs) `intrsex` (y:ys) | x<y  = xs `intrsex` (y:ys)
                            | x>y  = (x:xs) `intrsex` ys
                            | x==y = x:(xs `intrsex` ys)
    pens = map (\n->n*(3*n-1)`div`2) [1..]
    tris = map (\n->n*(n+1)`div`2) [1..]
    hexs = map (\n->n*(2*n-1)) [1..]

