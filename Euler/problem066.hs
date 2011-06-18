#!/usr/bin/runhaskell

import Data.Maybe
import Data.Function (on)
import Data.List ((\\),maximumBy)

d=61
x=1766319049
y=226153980
--d=7
--x=8
--y=3
main :: IO ()
main = print(extended_gcd d y)

extended_gcd a b
 | aModB==0 = (0,1)
 | otherwise = let (a', b') = extended_gcd b aModB
                   in (b', a'-(b'*aDivB))
 where (aDivB, aModB) = a`quotRem`b

--import System.Environment
--main = do (c:_) <- getArgs
--          print (soln (read c))

--(x+dy)(x-dy) = 1
--(x+1)(x-1) = dyy
--xx - Dyy = 1
--yy = (xx - 1)/D
--xx = 1 + Dyy
soln n = anAnswer 61--search [(makeDRec 61)] [2..]--(dRecsUnder n) [2..] --anAnswer 46--search (dsUnder n) (drop 1 squares)
 where
   
   ds = filter (not.isSquareGuess) [1..]
   primeDRecsUnder n = filter (\(_,_,isp)->isp) $ dRecsUnder n
   dRecsUnder n = map makeDRec $ takeWhile (<n) ds
   makeDRec d = (d, sqrt$fromInteger d, isPrime d)

   search dRecs (y:ys) | length dRecs <= 0 = (y*y,map (\(x,_,_)->x) dRecs)
   search [] _ = error "No maximum exists (there is a tie)"
   search dRecs (y:ys) = search dRecsLeft ys
     where dRecsLeft = filter (not.ok) dRecs
           ySq = y*y
           ok (d,dsqrt,True) = okPrime d dsqrt
           ok (d,dsqrt,False) = okComp d dsqrt
           
           --if d is prime, y^2=(x+1)(x-1)/d=n(nd+-2)
           --this is because either (x-1) or (x+1) must be divisible
           --by d since (x^2-1) is divisible by d. therefore, there must
           --exist an integer n that you can multipy d by to get either
           --(x+1) or (x-1).
           --if nd=x+1, nd(nd-2)=dy^2   n^2d-2n=y^2
           --if nd=x-1, nd(nd+2)=d^2    n^2d+2n=y^2
           okPrime d dsqrt =
             let n = y
                 nnd = n*n*d
                 n2 = n*2
                 nP = nnd+n2
                 nM = nnd-n2
                 guess = (fromInteger n)*dsqrt
                 in isSquare nP guess || isSquare nM guess
                 
           okComp d dsqrt =
             let n     = d*ySq
                 nFi   = fromInteger n
                 sqrtN = dsqrt*(fromInteger y)
                 guess = sqrtN*(nFi+1)/nFi
                 in isSquare (n+1) guess


   squares = map (^2) [1..]

   isSquare n nSqrtGuess = let x=(isqrt n nSqrtGuess) in x*x == n
   isSquare' n = let x=(round$sqrt$fromInteger n)in x*x == n --haskell sqrt
  
   isqrt n guess = isqrtNewton guess (fromInteger n)
   isqrtNewton x n | abs(xNext - x)<0.05 = floor xNext
                   | otherwise = isqrtNewton xNext n
     where xNext = (x/2) + n/(2*x)

   isSquareGuess n = isSquare n (sqrtApprox $ fromInteger n)
   sqrtApprox x | ds`mod`2 == 0 = 6*10^((ds-2)`div`2)
                | otherwise    = 2*10^((ds-1)`div`2)
     where ds = digs x
   digs = length . (takeWhile (>0)) . (iterate (`div`10))
 
-------------------------------
             --(x+1)(x-1) = dy^2
             --  so exactly one of x, x-1, or x+1 is divisible by 3
             --  that means that
             --  1) if x is divisible by 3,
             --       neither y or d is divisible by 3
             --       (since x+1 and x-1 arent divisible by 3)
             --  2) if x is not divisible by 3,
             --       either y or is divisible by three
             --       (since either x+1 or x-1 are divisible by 3)
             --  
             --(xSq`mult`3) /= (d`mult`3 || ySq`mult`3)
   primes = 2:filter isPrime [3..]
   isPrime n = n>1 && all (\x->n`mod`x/=0) (takeWhile (\p->p*p<=n) primes)

   primefactors x = pf x primes
   pf x (p:ps) | x==1  = []
               | x`mod`p == 0 = p:(pf (x`div`p) (p:ps))
               | otherwise    = pf x ps

   anAnswer d = head $ filter isSquare' $ map (\sq->d*sq+1) squares

   first f = head . (filter f)
   
   digs' x = length $ show x
   digs'' = (+) 1 . floor . logBase 10 . fromInteger

   isqrt''' x = floor $ sqrt $ fromInteger x

   isPerfectSq' x =
     let isq = isqrt''' x
         in isq*isq == x


   sqrt' :: Floating a => a -> a
   sqrt' s = exp (0.5 * (log s))

   frint = fromInteger
   isqrt' n = floor $ (flip (!!)) 4 $ iterate (isqrtNewton' (frint n)) (frint $ sqrtApprox n)
   isqrtNewton' n x = x - ((x*x - n)/(2*x))

   isqrt'' n = isqrtQuad (fromInteger n) (log (fromInteger n))*3^2
   isqrtQuad n prev = let next = (prev + (n/prev))/2
                          in if abs(next - prev) < 0.5
                             then floor next
                             else isqrtQuad n next

   isPerfectSq = isp . ispRange

   ispRange x | sqrtaSq < x =
                  (x, sqrta, first (\s->s*s>=x) $ succs (*2))
              | otherwise =
                  (x, first (\s->s*s<=x) $ succs (`div`2), sqrta)
    where sqrta = sqrtApprox x
          sqrtaSq = sqrta*sqrta
          succs f = drop 1 $ iterate f sqrta

   isp (x,min,max) | min > max = False
                   | x < midSq = isp (x,min,mid-1)
                   | x > midSq = isp (x,mid+1,max)
                   | otherwise = True
     where mid = (max+min)`div`2
           midSq = mid*mid

