-----------------------------------------------------------
--   Functional Programming -- Practical assignment 1    --
-----------------------------------------------------------
--                   ||  Getallen  ||                    --
-----------------------------------------------------------
-- Mark Montvai                - mmontvai - 3248852      --
-- Bart van Eck                - beck     - 3235785      --
-----------------------------------------------------------

module Getal where

-- Imported modules
import Char
import List

-----------------------------------------------------------
--                     Assignment 1                      --
-----------------------------------------------------------

-- The function uses foldl to add all 2 members of the list, the one to the left multiplied by 10 
fromDec :: [Int] -> Int
fromDec = foldl (\x y -> 10*x + y) 0

-----------------------------------------------------------
--                     Assignment 2                      --
-----------------------------------------------------------

-- The function quot sees how many times the second number fits in the first number, meaning that it take away the last number of an int
-- But before that happens the list gets filled up with the remaining number of the int when divided by 10, meaning the last number of the int
-- This is done recursevily until the list is completed
toDec :: Int -> [Int]
toDec 0 = []
toDec x = toDec (quot x 10) ++ [mod x 10]

-----------------------------------------------------------
--                     Assignment 3                      --
-----------------------------------------------------------

-- The function works the same way as fromDec, but now 2 is used instead of 10
fromBin :: [Int] -> Int
fromBin = foldl (\x y -> 2*x + y) 0

-----------------------------------------------------------
--                     Assignment 4                      --
-----------------------------------------------------------

-- The function works the same way as toDec, but now 2 is used instead of 10
toBin :: Int -> [Int]
toBin 0 = []
toBin x = toBin (quot x 2) ++ [mod x 2]

-----------------------------------------------------------
--                     Assignment 5                      --
-----------------------------------------------------------

-- The function recursively goes through every member of the list, calculating the matching number with the base number
fromBase :: Int -> [Char] -> Int
fromBase b [] = 0
fromBase b (x : xs) = res + fromBase b xs
                      where res = convertToInt b (toLower x) * (b^length xs)

-- Function that converts a char to the int, by using the char ASCII table and matching the right int to the right number
convertToInt :: Int -> Char -> Int
convertToInt b x = if isDigit x then digitToInt x 
                   else if ((ord x - 86) <= b) then (ord x - 87) 
                   else error "Convertion impossible with given basenumber"

-----------------------------------------------------------
--                     Assignment 6                      --
-----------------------------------------------------------

-- The function works kind of like toDec and toBin, picking the last digit of the int everytime and conerting it to the right char (that 
-- could also be a number). The converting is done in the convertToChar function
toBase :: Int -> Int -> [Char]
toBase b 0 = []
toBase b n = toBase b (quot n b) ++ res
             where res = [convertToChar(mod n b)]

-- Function that converts an int to the char, by using the char ASCII table and matching the right int to the right number
convertToChar :: Int -> Char
convertToChar x = if x > 9 then chr (x+87) 
                           else chr (x+48)

-----------------------------------------------------------
--                     Assignment 7                      --
-----------------------------------------------------------

-- The function first looks if the number can be converted. If it can, it is added to the list, converting it with the fromBase function
numbers :: Int -> [String] -> [(String, Int)]
numbers _ [] = []
numbers x (y : ys) = if possible x y then (y, fromBase x y) : numbers x ys 
                                     else numbers x ys

-- Function that returns whether a String can be converted to a number with a specified basenumber
possible :: Int -> String -> Bool
possible x [] = True
possible x (y:ys) = ord y - 87 <= x && possible x ys

-----------------------------------------------------------
--                     Assignment 8                      --
-----------------------------------------------------------
{-grayCode :: Int -> ([Char] -> Int,Int -> [Char])
grayCode :: -}

-- We couldn't figure out how to make this one, we found the assignment pretty hard to understand

-----------------------------------------------------------
--                     Assignment 9                      --
-----------------------------------------------------------

-- The function starts with adding its parameter to the list, and then passes the analyzed int on to the next recursive step. 
lookAndSay :: Int -> [String]
lookAndSay x = map intToDigit (toDec x) : lookAndSay (fromDec (analyse 1 (toDec x)))

-- Function that analyses a number in the look-and-say way. If to neighbouring digits match the countnumber (a) is raised by one.
-- When the function finds a pair that isnt the same, the first number is added to the list preceded by the countnumber
-- It recursively goed through the list that way
analyse :: Int -> [Int] -> [Int]
analyse a (x:[]) = a : [x]
analyse a (x:y:ys) = if (x == y) then analyse (a+1)(y:ys) 
                                 else a : x : analyse 1 (y:ys)

-----------------------------------------------------------
--                     Assignment 10                     --
-----------------------------------------------------------

-- The function first calls a small subfunction beginning at 10 (Keith numbers are higher then 9)
keithGetallen :: [Int]
keithGetallen = keithGetallen' 10
                where
				   -- The small subfunction checks if the parameter is a keithnumber, if so it adds it to the list, and then goes on to the next number
                   keithGetallen' :: Int -> [Int]
                   keithGetallen' x = if (isKeith (toDec x) (length (toDec x)) x) then x : keithGetallen' (x+1) 
				                                                                  else keithGetallen' (x+1)

-- Function that looks if a number is a Keithnumber. It adds the last y (length of original number) digits at the end of the list by dropping the rest,
-- and then looking if the number is the same as the original number or not. If it is, True is returned, if it's higher the search can be stopped 
-- and False is returned, and if its lower the same process happens again
isKeith :: [Int] -> Int -> Int -> Bool
isKeith x y z = if (sum == z) then True
                else if (sum >= z) then False
				else isKeith (x ++ [sum]) y z				 
				where sum = foldr (+) 0 (drop ((length x)-y) x)