-- | Ch04a

module Ch04a where

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = flip compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted = quicksort (filter (>x) xs)
    in  smallerSorted ++ [x] ++ biggerSorted

largestDivisible :: (Integral a) => a
largestDivisible = head (filter' p [10000,9999])
  where
    p y = y `mod` 3829 == 0

findSum :: Integer
findSum = sum $ takeWhile (<10000) $ filter odd $ map (^ 2) [1..]

findSum' :: Integer
findSum' = sum $ takeWhile (<10000) [x ^ 2 | x <- [1..], odd (x ^ 2)]

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain x
  | even x = x : chain (x `div` 2)
  | otherwise = x : chain (x * 3 + 1)

numLongChains :: Int
numLongChains = sum [1 | x <- [1..100], isLong $ chain x]
  where isLong xs = length xs > 15

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = \x y -> f y x

sum' :: (Num a) => [a] -> a
sum' = foldl (\a x -> a + x) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' e = foldl (\a y -> if e == y then True else a) False

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x a -> f x : a) [] xs

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\ x a -> if x > a then x else a)

reverse' :: [a] -> [a]
reverse' = foldl (\a x -> x : a) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter'1 :: (a -> Bool) -> [a] -> [a]
filter'1 p = foldr (\x a -> if p x then x : a else a) []

appl :: [Double]
appl = map ($ 3) [(4+), (10*), (^2), sqrt]

comp :: (Enum b, Num b) => [b]
comp = map (negate . sum . tail) [[1..5],[3..6],[1..7]]
