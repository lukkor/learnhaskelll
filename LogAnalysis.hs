{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

toInt :: String -> Int
toInt s = read s :: Int

parseMessage :: String -> LogMessage
parseMessage s = case (words s) of
                   ("I":t:m)   -> LogMessage Info (toInt t) (unwords m)
                   ("W":t:m)   -> LogMessage Warning (toInt t) (unwords m)
                   ("E":e:t:m) -> LogMessage (Error (toInt e)) (toInt t) (unwords m)
                   _           -> Unknown s

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert l Leaf           = Node Leaf l Leaf
insert l@(LogMessage _ t _) (Node left cl@(LogMessage _ ct _) rigth)
  | t < ct    = Node (insert l left) cl rigth
  | otherwise = Node left cl (insert l rigth)

build :: [LogMessage] -> MessageTree
build ls = foldr insert Leaf ls

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                = []
inOrder (Node left l rigth) = (inOrder left) ++ [l] ++ (inOrder rigth)

severityGreaterThanFifty :: LogMessage -> Bool
severityGreaterThanFifty (LogMessage (Error severity) _ _) = severity >= 50
severityGreaterThanFifty _                                 = False

getMessage :: LogMessage -> String
getMessage (LogMessage (Error _) _ m) = m
getMessage (LogMessage _ _ m)         = m
getMessage (Unknown _)                = "BUG_BUG_BUG"

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMessage . filter severityGreaterThanFifty . inOrder . build
