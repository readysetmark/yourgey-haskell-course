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
