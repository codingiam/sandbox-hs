-- | Ch04

module Ch04 where

gt100 :: Integer -> Bool
gt100 x = x > 100

greaterThan100 :: [Integer] -> [Integer]
greaterThan100 xs = filter gt100 xs

greaterThan100_2 :: [Integer] -> [Integer]
greaterThan100_2 = filter (\x -> x > 100)

greaterThan100_3 :: [Integer] -> [Integer]
greaterThan100_3 = filter (> 100)

foo :: (b -> c) -> (a -> b) -> (a -> c)
foo f g = \x -> f (g x)

myTest :: [Integer] -> Bool
myTest xs = even (length (greaterThan100 xs))

myTest' :: [Integer] -> Bool
myTest' = even . length . greaterThan100

f'' :: (Int, Int) -> Int
f'' (x, y) = 2 * x + y

foobar :: [Integer] -> Integer
foobar [] = 0
foobar (x:xs)
  | x > 3 = (7*x + 2) + foobar xs
  | otherwise = foobar xs

sum' :: [Integer] -> Integer
sum' [] = 0
sum' (x:xs) = x + sum' xs

product' :: [Integer] -> Integer
product' [] = 1
product' (x:xs) = x * product' xs

length' :: [a] -> Integer
length' [] = 0
length' (_:xs) = 1 + length' xs

fold :: b -> (a -> b -> b) -> [a] -> b
fold a _ [] = a
fold a f (x:xs) = fold (f x a) f xs

