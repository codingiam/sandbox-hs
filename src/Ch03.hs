-- | Ch03

module Ch03 where

data IntList = Empty | Cons Int IntList
             deriving Show

absAll :: IntList -> IntList
absAll Empty = Empty
absAll (Cons i is) = Cons (abs i) (absAll is)

squareAll :: IntList -> IntList
squareAll Empty = Empty
squareAll (Cons i is) = Cons (i * i) (squareAll is)

exampleList = Cons (-1) (Cons 2 (Cons (-6) Empty))

addOne x = x + 1
square x = x * x

mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList _ Empty = Empty
mapIntList f (Cons i is) = Cons (f i) (mapIntList f is)

keepOnlyEven :: IntList -> IntList
keepOnlyEven Empty = Empty
keepOnlyEven (Cons i is)
  | even i = Cons i (keepOnlyEven is)
  | otherwise = keepOnlyEven is

filterIntList :: (Int -> Bool) -> IntList -> IntList
filterIntList _ Empty = Empty
filterIntList f (Cons i is)
  | f i = Cons i (filterIntList f is)
  | otherwise = filterIntList f is

data List a = E | C a (List a)

lst1 :: List Int
lst1 = C 3 (C 5 (C 2 E))

filterList' :: (a -> Bool) -> List a -> List a
filterList' _ E = E
filterList' p (C x xs)
  | p x = C x (filterList' p xs)
  | otherwise = filterList' p xs

mapList' :: (a -> b) -> List a -> List b
mapList' _ E = E
mapList' f (C x xs) = C (f x) (mapList' f xs)
