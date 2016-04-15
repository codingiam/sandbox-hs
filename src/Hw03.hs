-- | Hw03

module Hw03 where

import Data.List (sort, group, intercalate,maximum,transpose)

skips :: [a] -> [[a]]
skips xs = map nths [1..(length xs)]
  where
    nxs = zip [1..] xs
    nths n = map snd $ filter (\(i, _) -> i `mod` n == 0) nxs

localMaxima :: [Integer] -> [Integer]
localMaxima (a:rs@(b:c:_))
  | b > a && b > c = b : localMaxima rs
  | otherwise = localMaxima rs
localMaxima _ = []

histogram :: [Integer] -> [Char]
histogram xs = intercalate "\n" (rotate histo)
  where
    ocs = map (pred . length) (group . sort $ [0..9] ++ xs)
    histo = map (\(i, c) -> show i ++ "-" ++ replicate c '*' ++ replicate (maximum ocs - c) ' ') (zip [0..] ocs)
    rotate = reverse . transpose
