-- CIS 194: Homework 2
-- http://www.seas.upenn.edu/~cis194/hw/01-intro.pdf

{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

-- Log File Parsing

parseMessage :: String -> LogMessage
parseMessage s = case words s of
					"I":ts:rest	  -> LogMessage Info (read ts) (unwords rest)
					"W":ts:rest   -> LogMessage Warning (read ts) (unwords rest)
					"E":l:ts:rest -> LogMessage (Error (read l)) (read ts) (unwords rest)
					msg	  		  -> Unknown (unwords msg)


parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)


-- Putting the logs in order

insert :: LogMessage -> MessageTree -> MessageTree
insert m Leaf 		= Node Leaf m Leaf
insert m@(LogMessage _ ts _) (Node l n@(LogMessage _ nts _) r)
	| ts < nts		= Node (insert m l) n r
	| otherwise		= Node l n (insert m r)
insert _ t 			= t


build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)


inOrder :: MessageTree -> [LogMessage]
inOrder Leaf		 = []
inOrder (Node l m r) = (inOrder l) ++ [m] ++ (inOrder r)


-- Log file postmortem

takeSeverity50plus :: [LogMessage] -> [String]
takeSeverity50plus [] = []
takeSeverity50plus ((LogMessage (Error s) _ msg):xs)
	| s >= 50	= msg : (takeSeverity50plus xs)
	| otherwise = takeSeverity50plus xs
takeSeverity50plus (_:xs) = takeSeverity50plus xs


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong m = takeSeverity50plus (inOrder (build m))