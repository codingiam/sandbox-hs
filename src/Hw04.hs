-- | Hw04

module Hw04 where

fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate itf
  where
    itf n | even n = n `div` 2
          | otherwise = 3 * n + 1

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr op Leaf
  where
    op x a =
      case a of
        Leaf -> Node 0 Leaf x Leaf
        Node _ l@(Leaf) v r@(Leaf) ->
          Node (dl + 1) nl v r
          where
            nl = op x l
            Node dl _ _ _ = nl
        Node _ l@(Node _ _ _ _) v r@(Leaf) ->
          Node (dr + 1) l v nr
          where
            nr = op x r
            Node dr _ _ _ = nr
        Node _ l@(Node ld _ _ _) v r@(Node rd _ _ _) ->
          if ld <= rd
          then Node (dl + 1) nl v r
          else Node (dr + 1) l v nr
          where
            nl = op x l
            nr = op x r
            Node dl _ _ _ = nl
            Node dr _ _ _ = nr
        Node _ l@(Leaf) v r@(Node _ _ _ _) ->
          Node (dl + 1) nl v r
          where
            nl = op x l
            Node dl _ _ _ = nl
