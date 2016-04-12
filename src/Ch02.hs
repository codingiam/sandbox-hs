-- | Ch02

module Ch02 where

data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
           deriving Show

listO'Things :: [Thing]
listO'Things = [Shoe, SealingWax, King, Cabbage, King]

isSmall :: Thing -> Bool
isSmall Shoe = True

isSmall2 :: Thing -> Bool
isSmall2 Ship = False
isSmall2 King = False
isSmall2 _ = True

data FailableDouble = Failure
                    | OK Double
                    deriving Show

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv a b = OK (a / b)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK a) = a

data Person = Person String Int Thing
            deriving Show

getAge :: Person -> Int
getAge (Person _ a _) = a

baz :: Person -> String
baz p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n

checkFav :: Person -> String
checkFav (Person n _ SealingWax) = n ++ ", you're my kind of person!"
checkFav (Person n _ _)          = n ++ ", your favorite thing is lame."

ex03 = case "Hello" of
  [] -> 3
  ('H':s) -> length s
  _  -> 7

failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
  Failure -> 0
  OK v -> v

data IntList = Empty | Cons Int IntList
intListProd :: IntList -> Int
intListProd Empty = 1
intListProd (Cons x l) = x * intListProd l

data Tree = Leaf Char
          | Node Tree Int Tree
          deriving Show
