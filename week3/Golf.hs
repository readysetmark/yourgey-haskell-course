-- CIS 194: Homework 3
-- http://www.seas.upenn.edu/~cis194/hw/03-rec-poly.pdf

{-# OPTIONS_GHC -Wall #-}
module Golf where
import Data.List


-- Exercise 1 Hopscotch

everyNth :: [a] -> Int -> [a]
everyNth xs nth = snd (unzip (filter (\ (x,_) -> x == 0) (map (\ (x,y) -> (x `mod` nth, y)) (zip [1..] xs))))

skips :: [a] -> [[a]]
skips [] = []
skips xs = map (\ n -> everyNth xs n) [1..(length xs)]


-- Exercise 2 Local Maxima

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima (x1:x2:x3:xs)
	| x2 > x1 && x2 > x3 = x2 : localMaxima (x2:x3:xs)
	| otherwise			 = localMaxima (x2:x3:xs)
localMaxima _ = []


-- Exercise 3 Histogram

