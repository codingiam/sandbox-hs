-- | Ch02a

module Ch02a where

import Data.List (sortBy, delete, nub)

data BookInfo = Book Int String [String]
              deriving (Show)

data MagazineInfo = Magazine Int String [String]
                  deriving (Show)

myInfo :: BookInfo
myInfo = Book 9780135072455 "Algebra of Programming"
  ["Richard Bird", "Oege de Moor"]

type CustomerID = Int
type ReviewBody = String

data BetterReview = BetterReview BookInfo CustomerID ReviewBody

type CardHolder = String
type CardNumber = Integer
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                 deriving (Show)

data Cartesian2D = Cartesian2D Double Double
                   deriving (Eq, Show)

data Polar2D = Polar2D Double Double
               deriving (Eq, Show)


data Roygbiv = Red
             | Orange
             | Yellow
             | Green
             | Blue
             | Indigo
             | Violet
             deriving (Eq, Show)

myNot :: Bool -> Bool
myNot True = False
myNot False = True

nicerID (Book id _ _) = id
nicerTitle (Book _ title _) = title
nicerAuthors (Book _ _ authors) = authors

goodExample :: [Int] -> Int
goodExample (x:xs) = x + goodExample xs
goodExample _ = 0

data Cusomer = Customer {
  customerID :: CustomerID
  , customerName :: String
  , customerAddress :: Address
  } deriving (Show)

customer = Customer {
  customerID = 271828
  , customerAddress = ["1048576 Disk Drive",
                       "Milpitas, CA 95134",
                       "USA"]
  , customerName = "Jane Q. Citizen"
  }

data List a = Cons a (List a)
            | Nil
              deriving (Show)

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

fromList :: List a -> [a]
fromList (Cons a ls) = a : fromList ls
fromList Nil = []

data MaybeTree a = MaybeNode (Maybe (a, Maybe (MaybeTree a), Maybe (MaybeTree a)))
                 deriving (Show)

safeSecond :: [a] -> Maybe a
safeSecond [] = Nothing
safeSecond xs = if null (tail xs)
                then Nothing
                else Just (head (tail xs))

lend :: Integer -> Integer -> Maybe Integer
lend amount balance = let reserve = 100
                          newBalance = balance - amount
                      in if balance < reserve
                         then Nothing
                         else Just newBalance

lend2 :: Integer -> Integer -> Maybe Integer
lend2 amount balance = if balance < reserved
                       then Nothing
                       else Just newBalance
  where
    reserved = 100
    newBalance = balance - amount

pluralise :: String -> [Int] -> [String]
pluralise word counts = map plural counts
  where plural 0 = "no " ++ word ++ "s"
        plural 1 = "one " ++ word
        plural n = show n ++ " " ++ word ++ "s"

fromMaybe :: a -> Maybe a -> a
fromMaybe defval wrapped =
  case wrapped of
    Nothing -> defval
    Just value -> value

lend3 :: (Fractional a, Ord a) => a -> a -> Maybe a
lend3 amount balance
  | amount <= 0            = Nothing
  | amount > reserve * 0.5 = Nothing
  | otherwise              = Just newBalance
  where reserve    = 100
        newBalance = balance - amount

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length xs

mean' :: Integral a => [a] -> Double
mean' [] = error "Empty list"
mean' xs = sumxs / lenxs
  where
    sumxs = fromIntegral (sum xs)
    lenxs = fromIntegral (length xs)

toPalindrome xs = xs ++ reverse xs

isPalindrome xs = xs == reverse xs

sortByLength xs = sortBy (\ys zs -> compare (length ys) (length zs)) xs

intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse _ (xs:[]) = xs
intersperse s xs = head xs ++ [s] ++ intersperse s (tail xs)

treeHeight :: Tree a -> Int
treeHeight (Empty) = 0
treeHeight (Node _ left right) = 1 + max (treeHeight left) (treeHeight right)

data Direction = GoLeft
               | GoRight
               | GoStraight
               deriving (Show, Eq)

data Point = Point Double Double
           deriving (Show, Eq)

dotProduct :: Point -> Point -> Point -> Double
dotProduct (Point ax ay) (Point bx by) (Point cx cy) = bax * bcx + bay * bcy
  where bax = ax - bx
        bay = ay - by
        bcx = cx - bx
        bcy = cy - by

crossProductLength :: Point -> Point -> Point -> Double
crossProductLength (Point ax ay) (Point bx by) (Point cx cy) = bax * bcy - bay * bcx
  where bax = ax - bx
        bay = ay - by
        bcx = cx - bx
        bcy = cy - by

angle :: Point -> Point -> Point -> Double
angle a b c = atan2 (crossProductLength a b c) (dotProduct a b c)

turn :: Point -> Point -> Point -> Direction
turn a b c | cpl < 0.0 = GoLeft
           | cpl > 0.0  = GoRight
           | otherwise = GoStraight
  where
    cpl = crossProductLength a b c

directions :: [Point] -> [Direction]
directions (a:b:c:[]) = [turn a b c]
directions (a:b:c:xs) = (turn a b c) : directions (b : c : xs)
directions _ = []

grahamScan :: [Point] -> [Point]
grahamScan ss
  | length ss >= 3 = scan [pt0] rests
  | otherwise = ss
  where
    pt0 = foldr leftmostBottom (Point (1/0) (1/0)) ss
      where
        leftmostBottom x a = let Point _ xy = x
                                 Point _ ay = a
                             in
                               if xy <= ay then x else a

    rests = sortBy comp (delete pt0 (nub ss))
      where
        ang (Point ax ay) (Point bx by) = (bx - ax) / (by - ay)
        comp x y = compare (ang pt0 y) (ang pt0 x)

    scan [p0] (p1:[p2])
      | turn p0 p1 p2 == GoStraight = [p0, p2]

    scan (x:xs) (y:z:rsts)  = case turn x y z of
      GoRight -> scan xs (x:z:rsts)
      GoStraight -> scan (x:xs) (z:rsts) -- skip collinear points
      GoLeft -> scan (y:x:xs) (z:rsts)

    scan xs [z] = z : xs
