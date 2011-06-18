#!/usr/bin/runhaskell

main :: IO ()
main = print(soln)

-- for any x>=1:
--   x is an nth power: x = k^n, k>=1
--   x is an n-digit number: n = floor(lg x) + 1
--                                {lg is log base 10}

-- x = k^(floor(lg x) + 1)
-- lg x = lg(k^(floor(lg x) + 1))
-- lg x = (floor (lg x) + 1)(lg k)
-- (lg x) / (floor (lg x) + 1) = lg k
--    lg x < floor (lg x) + 1
--    (lg x) / (floor (lg x) + 1) < 1
--    lg k < 1
--    10^(lg k) < 10^1
--    k < 10
--    1<=k<=9

-- x <= 9^n,   since k<=9
-- n <= 1 + floor(lg 9^n)
-- n <= 1 + lg 9^n,  since n is an integer
-- n <= 1 + n(lg 9)
-- n-(lg 9)n <= 1
-- n <= 1/(1-lg 9)
-- 1<=n<=21

soln = sum $ map (length.powdigs) [1..21] --1<=n<=21
 where
   powdigs n = filter ((==n).digs) $ map (^n) [1..9] --1<=k<=9

   digs c = length $ takeWhile (>0) $ iterate (`div`10) c

