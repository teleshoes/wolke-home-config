#!/usr/bin/runhaskell

main :: IO ()
main = print(soln 50)

soln n = primes !! n
 where
   primes = 2:(filter isPrime [3,5..])
   isPrime n = not $ any (mult n) (takeWhile (\p->p*p<=n) primes)
   a `mult` b = a`mod`b==0

