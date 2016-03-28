-- | Ch01

module Ch01 where

hailStone :: Integer -> Integer
hailStone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise = 3 * n + 1

foo :: Integer -> Integer
foo 0 = 16
foo 1
  | "Haskell" > "C++" = 3
  | otherwise = 4
foo n
  | n < 0 = 0
  | n `mod` 17 == 2 = -43
  | otherwise = n + 3

sumPair :: (Int, Int) -> Int
sumPair (x, y) = x + y

hailStoneSeq :: Integer -> [Integer]
hailStoneSeq 1 = [1]
hailStoneSeq n = n : hailStoneSeq (hailStone n)
