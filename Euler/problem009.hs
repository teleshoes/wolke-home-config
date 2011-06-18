#!/usr/bin/runhaskell

main :: IO ()
main = print(soln 1000)

soln sum = findtrip [(a,b,(getc a b)) | a<-[1..sum], b<-[1..sum]]
  where findtrip [] = error ("couldnt find a pythag triple a^2+b^2=c^2 where a+b+c=" ++ show(sum))
        findtrip ((_,_,Nothing):xs) = findtrip xs
        findtrip ((a,b,Just c):xs) | a+b+c==sum = a*b*c
                                   | otherwise = findtrip xs
        getc a b = perfectsqrt (a*a + b*b)
        perfectsqrt x = let root = floor (sqrt (fromRational(toRational (fromIntegral x))))
                        in if (root*root) == x then Just root else Nothing





stupid = prodTrip $ head $ filter isPythagTrip (genTrips 1000)

prodTrip (x,y,z,sum) = x*y*z

genTrips :: Int -> [(Int, Int, Int, Int)]
genTrips max = genTripsh1 [1..max]
genTripsh1 [] = []
genTripsh1 (x:xs) = (genTripsh2 x xs) ++ (genTripsh1 xs)
genTripsh2 _ [] = []
genTripsh2 x (y:ys) = (genTripsh3 x y ys) ++ (genTripsh2 x ys)
genTripsh3 _ _ [] = []
genTripsh3 x y (z:zs) = (x,y,z, x+y+z) : (genTripsh3 x y zs)

genTripsh [] _ _ = []
genTripsh _ [] _ = []
genTripsh _ _ [] = []
genTripsh (x:xs) (y:ys) (z:zs) = (x,y,z, x+y+z) :
                                 ((genTripsh xs     (y:ys) (z:zs)) ++
                                  (genTripsh (x:xs) ys     (z:zs)) ++
                                  (genTripsh (x:xs) (y:ys) zs))

isPythagTrip :: (Int, Int, Int, Int) -> Bool
isPythagTrip (a,b,c,1000) = (a^2+b^2) == c^2
isPythagTrip _ = False
--isPythagTrip (a,b,c,_) = (a^2+b^2) == c^2

