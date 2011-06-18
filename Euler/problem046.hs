#!/usr/bin/runhaskell

main :: IO ()
main = print(soln)

soln = head $ dropWhile works compodds
 where 
   works c = any (findPrime c) (takeWhile (<c) dblSquares)
   findPrime c d = c-d == (head $ dropWhile (<c-d) primes)

   compodds = filter (not . isPrime) [3,5..]
   dblSquares = map ((*2).(^2)) [1..]
   primes = 2:(filter isPrime [3,5..])
   isPrime n = not $ any (mult n) (takeWhile (\p->p*p<=n) primes)
   a `mult` b = a`mod`b==0

