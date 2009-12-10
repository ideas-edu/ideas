-- Practicum 1: Getallen
-- Marc Vaarties (mrvaarti 3344959)
-- Rob van de Werken (rwerken 3363066)
module Getallen where

import Data.Char
import Data.List

------------------------------------------------------------------------
--------------------------- helper functions ---------------------------
------------------------------------------------------------------------
--select function to use for from(option 2)
from :: Int -> [Int] -> Int
from a b | (or . (map (a<))) b = error "[Int]->Int conversion out of range"
		 | otherwise         = from'' a b

--option 1
from' :: Int -> [Int] -> Int
from' a b = sum (zipWith (\x y -> x*y) b (zipWith (\x y -> y^x) [0 ..] (repeat a) ))

--option 2 this is the fastest
from'' :: Int -> [Int] -> Int
from'' a [] = 0
from'' a [b] = b
from'' a ( kop : tussen : staart ) = from'' a ([(kop * a) + tussen ] ++ staart)

--option 3
from''' :: Int -> [Int] -> Int
from''' a b = foldl (\x y -> x*a+y) 0 b

to'  :: Int -> Int -> [Int]
to' a 0 = []
to' a b =  (to' a (b `div` a)) ++ [ b `rem` a ]

--function to convert a character to int
charToInt :: Char -> Int
charToInt c
  | isDigit c            = ord c - ord '0'
  | c >= 'a' && c <= 'z' = ord c - ord 'a' + 10
  | c >= 'A' && c <= 'Z' = ord c - ord 'A' + 10

--function to convert a int to character
intToChar :: Int -> Char
intToChar c
  | c <=  9   = chr (c + ord '0')
  | otherwise = chr (c + ord 'a' - 10)

--output a list of characters representing a gray code
toGray :: Int -> Int -> [Char]
toGray a b = toBase a (fromBin (head c:(zipWith xor (tail c) c)))
	where c = toBin b

--output a int representing a given gray code
fromGray :: Int -> [Char] -> Int
fromGray a b = fromBin(fromBinGray (toBin (fromBase a b)))

--function to do first step of gray code recursion and start rest of recursion
fromBinGray :: [Int] -> [Int]
fromBinGray []     = []
fromBinGray (x:xs) = x:(fromBinGray' x xs)

--function to do the rest of gray code recursion
fromBinGray' :: Int -> [Int] -> [Int]
fromBinGray' p [] = []
fromBinGray' p (x:xs) = b:fromBinGray' b xs
	where b = xor p x

--function to do an xor operation on a int (and only value 1)
xor :: Int -> Int -> Int
xor 1 0 = 1
xor 0 1 = 1
xor _ _ = 0

--function to check if the parameters represent a keith number
keithCheck :: Int -> [Int] -> Bool
keithCheck a b | a >  c    = keithCheck a ((sum b):(init b))
			   | a == c    = True
			   | otherwise = False
					where c = head b

------------------------------------------------------------------------
------------------------- functions for tasks --------------------------
------------------------------------------------------------------------

--opgave 1
fromDec :: [Int] -> Int
fromDec a = from 10 a

--opgave 2
toDec :: Int -> [Int]
toDec a = to' 10 a

--opgave 3
fromBin :: [Int] -> Int
fromBin a = from 2 a

--opgave 4
toBin :: Int -> [Int]
toBin a = to' 2 a

--opgave 5
fromBase :: Int -> [Char] -> Int
fromBase a b = from a (map charToInt b)

--opgave 6
toBase :: Int -> Int -> [Char]
toBase a b = map intToChar (to' a b)

--opgave 7
numbers :: Int -> [ String ] -> [(String, Int)]
numbers a b = map (\x -> (x,(fromBase a x))) b

--opgave 8
grayCode :: Int -> ([Char]->Int, Int->[Char])
grayCode a = ((fromGray a), (toGray a))

--opgave 9
lookAndSay :: Int -> [String]
lookAndSay a = b:lookAndSay' b
     where b = show a

lookAndSay' :: String -> [String]
lookAndSay' a = c:lookAndSay' c
      where c = concat (zipWith (++) (map (show.length) d) (map nub d))
            d = group a

--opgave 10
keithGetallen :: [Int]
keithGetallen = filter (\x -> keithCheck x (reverse (toDec x))) [14..]
