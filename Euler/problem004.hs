#!/usr/bin/runhaskell

main :: IO ()
main = print(unjust $ firstPalindrome (mergesort gt (prods (intsOfLength 3))))

unjust (Just x) = x
unjust Nothing = error "Nothing"

lt :: Int -> Int -> Int
lt x y | x < y = 1
       | x == y = 0
       | x > y = 0-1

gt :: Int -> Int -> Int
gt x y | x > y = 1
       | x == y = 0
       | x < y = 0-1

mergesort :: (a -> a -> Int) -> [a] -> [a]
mergesort _ [] = []
mergesort _ (x:[]) = [x]
mergesort cmp xs = merge cmp (mergesort cmp one) (mergesort cmp two)
             where lists = split xs
                   one = fst lists
                   two = snd lists

split :: [a] -> ([a], [a])
split xs = splith xs [] []
splith [] xs ys = (xs, ys)
splith (x:y:zs) xs ys = splith zs (x:xs) (y:ys)
splith (x:zs) xs ys = splith zs (x:xs) ys


merge :: (a -> a -> Int) -> [a] -> [a] -> [a]
merge _ [] [] = []
merge _ [] ys = ys
merge _ xs [] = xs
merge cmp (x:xs) (y:ys) | (cmp x y) >= 0 = x : merge cmp xs (y:ys)
                        | otherwise      = y : merge cmp (x:xs) ys


prods :: [Int] -> [Int]
prods [] = []
prods (x:xs) = (map (*x) (x:xs)) ++ (prods xs)

pow :: Int -> Int -> Int
pow x n | n < 0     = error "Non-negative exponents only"
        | n == 0    = 1
        | otherwise = x * pow x (n-1)

intsOfLength :: Int -> [Int]
intsOfLength n = [smallest..largest]
               where smallest = 10 `pow` (n-1)
                     largest = (10 `pow` n)-1


concat' :: [a] -> a -> [a]
concat' [] e = [e]
concat' (x:xs) e = x : (concat' xs e)

reverse' :: [a] -> [a]
reverse' (x:xs) = concat' (reverse' xs) x

equals' :: [Int] -> [Int] -> Bool
equals' [] [] = True
equals' [] _ = False
equals' _ [] = False
equals' (x:xs) (y:ys) | (x == y)  = equals' xs ys
                      | otherwise = False

digits :: Int -> [Int]
digits x | x<=0      = error "Positive integers only"
         | otherwise = digitsh x []
digitsh 0 ds = ds
digitsh x ds = digitsh (x `div` 10) ((x `mod` 10):ds)


undigits :: [Int] -> Int
undigits xs = undigitsh (reverse xs) 1 0
undigitsh [] _ sum = sum
undigitsh (d:ds) place sum | d<0 || d>9 = error "is not a digit"
                           | otherwise = undigitsh ds (place*10) (sum + (d*place))

isPalindrome :: Int -> Bool
isPalindrome x = isPalindromeh (digits x)
isPalindromeh xs = equals' xs (reverse xs)

firstPalindrome :: [Int] -> Maybe Int
firstPalindrome [] = Nothing
firstPalindrome (x:xs) | isPalindrome x = Just x
                       | otherwise      = firstPalindrome xs

