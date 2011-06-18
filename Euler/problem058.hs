#!/usr/bin/runhaskell

main :: IO ()
main = print(soln)

soln = fst $ head $ dropWhile (\x->percentFloor x>=10) diagRatio
 where
   diagRatio = drop 1 $ iterate addNext (1,0)
   percentFloor (len, pCount) = (100*pCount)`div`(len*2-1)
   addNext (len, pCount) =
     let next = corners (len+2)
     in (len+2, pCount + (length $ filter isPrime next))
   corners n = take 4 $ iterate ((flip (-)) (n-1)) (n*n)

   primes = 2:(filter isPrime [3,5..])
   isPrime n = not $ any (mult n) (takeWhile (\p->p*p<=n) primes)
   a `mult` b = a`mod`b==0

