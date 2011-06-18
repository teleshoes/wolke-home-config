#!/usr/bin/runhaskell

main :: IO ()
main = print(soln 4)

soln n = head $ consec n ((n==) . (dpfCount 0 primes)) [1..] []
 where
   consec n f (x:xs) cs
    | n==length cs = reverse cs
    | f x = consec n f xs (x:cs)
    | otherwise = let guess = drop (n-1) xs
                  in consec n f (if f(head guess) then xs else guess) []

   dpfCount count (p:ps) x
     | x<p = count
     | x`mult`p = dpfCount (count+1) ps (x`div`p)
     | otherwise = dpfCount count ps x

   primes = 2:(filter isPrime [3,5..])
   isPrime n = not $ any (mult n) (takeWhile (\p->p*p<=n) primes)
   a `mult` b = a`mod`b==0


--isSquareOfPrime n = elemSorted n sqPrimes
--sqPrimes = map (^2) primes
--elemSorted n (x:xs) | n<x = False
--                    | n==x = True
--                     | otherwise = elemSorted n xs

