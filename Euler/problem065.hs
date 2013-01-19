#!/usr/bin/runhaskell

import Data.Char (digitToInt)

main :: IO ()
main = print(soln 100)

soln n = sum $ digits $ fst $ convergent seqE n
 where
   convergent seq n = let seqs = reverse $ take n seq
                           in approx ((head seqs),1) (tail seqs)

   approx (top,btm) (x:xs) = approx (x*top+btm,top) xs
   approx ratio [] = ratio

   seqE = 2:(concatMap (\k->[1,2*k,1]) [1..])
   seq2 = 1:(cycle [2])

   digits x = map (digitToInt) $ show x

