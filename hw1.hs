-- CIS 194: Homework 1
-- http://www.seas.upenn.edu/~cis194/hw/01-intro.pdf

-- Exercises 1-4 build a credit card validator

-- Exercise 1

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
	| n <= 0	= []
	| otherwise = (n `mod` 10) : (toDigitsRev (n `div` 10))

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)


-- Exercise 2

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] 	= []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x1:x2:xs)
	| odd (length xs)	= x1 : (x2 * 2) : (doubleEveryOther xs)
	| otherwise			= (x1 * 2) : x2 : (doubleEveryOther xs)


-- Exercise 3

sumDigits :: [Integer] -> Integer
sumDigits [] 	 = 0
sumDigits (x:[])
	| length (show x) > 1 = sumDigits (toDigits x)
	| otherwise			  = x
sumDigits (x:xs)
	| length (show x) > 1 = (sumDigits (toDigits x)) + (sumDigits xs)
	| otherwise			  = x + (sumDigits xs)


-- Exercise 4

validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0
