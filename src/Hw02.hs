 -- | Hw02

module Hw02 where

import Log

parseMessage :: String -> LogMessage
parseMessage s = makeMessage (words s)
  where
    makeMessage ("I":timestamp:ss) = LogMessage Info (toI timestamp) (unwords ss)
    makeMessage ("W":timestamp:ss) = LogMessage Warning (toI timestamp) (unwords ss)
    makeMessage ("E":severity:timestamp:ss) = LogMessage (Error (toI severity)) (toI timestamp) (unwords ss)
    makeMessage _ = Unknown s

    toI str = read str :: Int

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert m@(LogMessage _ ms _) t =
  case t of
    (Node tl v@(LogMessage _ ts _) tr) -> if ts > ms
                                          then Node (insert m tl) v tr
                                          else Node tl v (insert m tr)
    (Node _ (Unknown _) _) -> error "what?"
    Leaf -> Node Leaf m Leaf

build :: [LogMessage] -> MessageTree
build xs = foldl doInsert Leaf xs
  where
    doInsert t m = insert m t

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l v r) = (inOrder l) ++ [v] ++ (inOrder r)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong ((LogMessage (Error c) _ s):xs) = if c > 50
                                                then s : whatWentWrong xs
                                                else whatWentWrong xs
whatWentWrong (_:xs) = whatWentWrong xs
