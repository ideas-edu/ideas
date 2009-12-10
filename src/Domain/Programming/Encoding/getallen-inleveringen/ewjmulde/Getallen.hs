{-
  ----------------------------
  | Functioneel Programmeren |
  | Practicum 1: Getallen    |
  |--------------------------|
  | Naam: Erik Mulder        |
  | Studentnr: 3242951       |
  ----------------------------
  | Compiler: Helium 1.6     |
  ----------------------------
-}

module Getallen where

-- This import is needed for the 'ord' en 'chr' functions.
import Char

--------------------------
-- Assignment functions --
--------------------------

-- Assignment 1.
-- Takes a list of decimals and produces corresponding number. This function
-- uses the more general function 'fromNumeralSystem' with '10' as base.
fromDec :: [Int] -> Int
fromDec = fromNumeralSystem 10

-- Assignment 2.
-- Takes a number and produces the corresponding list of decimals. This
-- function uses the more general function 'toNumeralSystem' with '10' as base.
toDec :: Int -> [Int]
toDec = toNumeralSystem 10

-- Assignment 3.
-- Takes a list of bits and produces corresponding decimal number. This
-- function uses the more general function 'fromNumeralSystem' with '2' as base.
fromBin :: [Int] -> Int
fromBin = fromNumeralSystem 2

-- Assignment 4.
-- Takes a number and produces the corresponding list of bits. This function
-- uses the more general function 'toNumeralSystem' with '2' as base.
toBin :: Int -> [Int]
toBin = toNumeralSystem 2

-- Assignment 5.
-- Takes a base and a list of chars and produces the corresponding decimal
-- number. This function uses the more general function 'fromNumeralSystem',
-- after converting the characters to their numerical counterparts.
fromBase :: Int -> [Char] -> Int
fromBase base = (fromNumeralSystem base).map (charToIntBase base)

-- Assignment 6.
-- Takes a base and a number and produces the corresponding list of characters.
-- This function uses the more general function 'toNumeralSystem', converting
-- the numbers to their character counterparts afterwards.
toBase :: Int -> Int -> [Char]
toBase base = (map (intToCharBase base)).toNumeralSystem base

-- Assignment 7.
-- Takes a base and a list of words and produces tuples of all words with their
-- numeric representation according to the given base. Words that have illegal
-- characters (according to the given base) are left out.
-- N.B. This latter check is already incorporated in the 'fromBase' function,
-- but produces an error there. So we needed to add a silent ignore check here.
numbers :: Int -> [String] -> [(String, Int)]
numbers base = foldr getTuple []
    where getTuple number = if notAboveBase number then (tuple:) else id
              where notAboveBase = and.map ((< base).charToInt)
                    tuple = (number, fromBase base number)

-- This function performs the actual conversion algorithm and is used in
-- assignments 1, 3 and 5. The algorithm consists of multiplying the
-- accumulator with the base and adding the current item. The input is also
-- checked on validity.
fromNumeralSystem :: Int -> [Int] -> Int
fromNumeralSystem base input
    | and (map (<base) input) = foldl ((+).(base*)) 0 input
    | otherwise = error "Input not within limits of base"

-- This function performs the actual conversion algorithm and is used in
-- assignments 2, 4 and 6. The algorithm consists of dividing the input by the
-- base, adding the rest to the answer and proceeding with the result as input.
toNumeralSystem :: Int -> Int -> [Int]
toNumeralSystem base = toNumeralSystem' []
    where toNumeralSystem' [] 0 = [0]
          toNumeralSystem' acc 0 = acc
          toNumeralSystem' acc number = toNumeralSystem' (rest:acc) result
              where (result, rest) = divMod number base

-- Assignment 8.
-- Combines the 'fromGrayCode' and 'toGrayCode' functions with the given base.
grayCode :: Int -> ([Char] -> Int, Int -> [Char])
grayCode base = (fromGrayCode base, toGrayCode base)

-- Converts a gray code to a decimal number of the given base.
-- Algorithm is based on the description found in: http://www.library.tudelft.
-- nl/ws/search/publications/search/metadata/index.htm?docname=162885 page 17.
-- Which in turn took it's description from: Sharma, B.D. and R. K. Khanna,
-- "On m-ary Gray codes", Information Sciences, vol. 15, pp. 31-43, 1978.
fromGrayCode :: Int -> [Char] -> Int
fromGrayCode base input = fromNumeralSystem base (fromGrayCode' 0 inputAsList)
    where inputAsList = map (charToIntBase base) input
          fromGrayCode' _ [] = []
          fromGrayCode' y (x:xs) = current:fromGrayCode' current xs
              where current = (x + y) `mod` base

-- Converts a decimal number to a gray code of the given base.
-- Algorithm is based on the description found in: http://www.library.tudelft.
-- nl/ws/search/publications/search/metadata/index.htm?docname=162885 page 17.
-- Which in turn took it's description from: Sharma, B.D. and R. K. Khanna,
-- "On m-ary Gray codes", Information Sciences, vol. 15, pp. 31-43, 1978.
toGrayCode :: Int -> Int -> [Char]
toGrayCode base input = map (intToCharBase base) (toGrayCode' 0 inputAsList)
    where inputAsList = toNumeralSystem base input
          toGrayCode' _ [] = []
          toGrayCode' y (x:xs) = ((x - y) `mod` base):toGrayCode' x xs

-- Assignment 9.
-- Creates an infinite list of Strings of numbers describing the previous entry,
-- by "saying" the sequence of numbers, e.g. input "2311" gives "121321" as
-- output for "one 2, one 3 and two 1's". The first argument is the start input
-- number. Works by putting the current number (as a String) in front of the
-- recursive call with the next 'said' number. The saying is done by a counter
-- function that creates the result from the end backwards, increasing the
-- count if the same number is encountered and prepending a new count of 1 if
-- a new number is encountered.
lookAndSay :: Int -> [String]
lookAndSay = lookAndSay'.(toNumeralSystem 10)
    where lookAndSay' input = (map intToChar input):lookAndSay' nextEntry
              where nextEntry = foldr counter [] input
                    counter x (c:n:cns) = if x == n
                                          then (c+1):n:cns
                                          else 1:x:c:n:cns
                    counter x [] = [1, x]
                    counter _ [_] = error "Not possible" 

-- Assignment 10.
-- Generates an infinite list of all Keith numbers. These Keith numbers are
-- filtered from the Keith sequences of the integers from 10 onwards. A Keith
-- number is a number that is present in it's own keith sequence. This check
-- is done by the helper function 'elemAsc'.
keithGetallen :: [Int]
keithGetallen = filter keithGetal [10..]
    where keithGetal i = elemAsc i (keithReeks i)

-- Generates the infinite Keith sequence of a given number. Works by starting
-- with the input numbers and for every next number adding the n previous
-- numbers, where n is the length of the input. This can elegantly be done
-- with an addition zipWith of n iterations of the tail of the same list.
keithReeks :: Int -> [Int]
keithReeks input = start ++ foldr (zipWith (+)) (repeat 0) tailLists
    where start = (toNumeralSystem 10 input)
          tailLists = take (length start) (iterate tail (keithReeks input))


----------------------
-- Helper functions --
----------------------

-- Combines a 'div' and a 'mod' and returns the result as a tuple.
-- This function is present in the GHC prelude, but not in Helium, so it's
-- added here manually.
divMod :: Int -> Int -> (Int, Int)
divMod x y = (x `div` y, x `mod` y) 

-- Converts a character to a number, following and extending the hexadecimal
-- style: '1' = 1 .. '9' = 9, 'a' = 10, 'b' = 11, etc. until 'z' = 35.
-- Works for lower and upper case characters.
charToInt :: Char -> Int
charToInt char
    | isDigit char = index - ord '0'
    | isUpper char = index - ord 'A' + 10
    | isLower char = index - ord 'a' + 10
    | otherwise = error "No conversion possible"
    where index = ord char

-- Converts a number to a character, following and extending the hexadecimal
-- style: 1 = '1' .. 9 = '9', 10 = 'a', 11 = 'b', etc. until 35 = 'z'.
-- Output character is in lower case.
intToChar :: Int -> Char
intToChar int
    | int >= 0 && int <= 9 = chr (int + ord '0')
    | int >= 10 && int <= 35 = chr (int + ord 'a' - 10)
    | otherwise = error "No conversion possible"

-- Converts a character to a number using 'charToInt' and also checks
-- if the resulting number does not conflict with the base.
charToIntBase :: Int -> Char -> Int
charToIntBase base char
    | result < base = result
    | otherwise = error "Char not within limits of base"
    where result = charToInt char

-- Converts a number to a character using 'intToChar' and also checks
-- if the input number does not conflict with the base.
intToCharBase :: Int -> Int -> Char
intToCharBase base int
    | int < base = intToChar int
    | otherwise = error "Int not within limits of base"

-- Searches for an item in a sorted, ascending list.
-- When the next item in the list is greater than the searched item,
-- the function returns False. Therefore, this function can also be
-- used on infinite lists and always returns an answer, as opposed to 'elem'.
-- N.B. A binary search will not work here because we have to be able
-- to handle infinite lists, which by definition have no 'halfway'.
-- N.B.2 With effort, a dense, infinite list in ascending order could be
-- created that will still get this function in an inifite loop.
-- For instance: elemAsc [2] (iterate (1:) [1])
-- N.B.3 There seems to be a bug in Helium on comparing lists, preventing
-- this method from working correctly on lists.
-- I filed a bug report and the bug is confirmed by 'jur@cs.uu.nl'.
-- The bug is no problem for the way this function is used in the
-- assignment functions.
elemAsc :: Ord a => a -> [a] -> Bool
elemAsc _ [] = False
elemAsc x (y:ys)
    | x > y = elemAsc x ys
    | x == y = True
    | otherwise = False
