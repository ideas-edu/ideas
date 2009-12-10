{- Functioneel programmeren
-- Practicum 1: Getallen
-- Author: O.M.Penningnieuwland
-- Written for GHC 6.10.1
-- For the assignment, see http://www.cs.uu.nl/wiki/pub/FP/Practicum/getallen.pdf
-- 
-- Lots of functions that do all sorts of weird stuff with numbers.
-}

import Char
import Maybe

{- Generally helpful functions for the assignments -}

-- Return the number represented by the array of integers in a given base. Used in assignments 1, 3, 5 and 8.
fromAap :: Int -> [Int] -> Int
fromAap base = foldl (nextInt base) 0
             where nextInt base x y | y >= base = error (concat["fromAap: Number ", show y, " out of range for base ", show base,"."])
                                    | y < 0 = error (concat["fromAap: Number ", show y, " is negative."])
                                    | otherwise = x * base + y
            -- nextInt throws an error for y >= base, as y:_ would not be a correct representation of a number in the given base.
            -- For instance, [42,1,0,1] is not a correct binary representation of any number (but would yield 341 without the error).
            -- An error is also thrown for y < 0, since allowing negative integers would allow different representations of the same number.
            -- For instance, base 10 [1,3,3,7] would be equal to base 10 [1,-9,3,3,7], both yielding 1337 as result.
            -- Note that this also guarantees that base <= 0 is illegal, as it will be impossible to get 0 <= y < base.


-- Return an array of integers representing a given number in a given base. Used in assignments 2, 4, 6 and 8.
-- Note: GHC seems to blatantly ignore Int overflows and give a result anyway, wrapping input around to fit in an Int.
-- As a result, expect weird results for input > 2147483647 or < (-2147483648) when the input is of type Int.
-- For instance, toDec 2165432265542532635 gives [7,5,2,1,5,6,1,8,7] as result, because toDec provides an Int as input for toAap.
toAap :: Integral a => a -> a -> [a]
toAap base x =  toAapAccum base x []
             where toAapAccum base x result | x < 0 = error "toAap: Undefined for negative integers."
                                            | x <= (base-1) = x:result
                                            | otherwise = toAapAccum base (x `div` base) ((x `mod` base):result)

-- Convert the alphanumerical characters [0..9]++[a..z] to the integers they represent. Used in assignments 5 and 8.
charToIntAap :: Char -> Int
charToIntAap x | xInAscii >= 48 && xInAscii <= 57 = xInAscii - 48
               | xInAscii >= 97 && xInAscii <= 122 = xInAscii - 87
               | otherwise = error "charToIntAap: Character input must be lowercase alphanumerical."
               where xInAscii = ord x

-- Convert the integers [0..35] to the alphanumerical characters that represent them. Used in assignments 6 and 8.
intToCharAap :: Int -> Char
intToCharAap x | x>=0 && x<=9 = chr (48+x)
               | x>9 && x<=35 = chr (87+x)
               | otherwise = error "intToCharAap: Integer input must be between 0 and 35 (inclusive)."

-- Similar to dropWhile, but also return the number of elements dropped. Used in assignment 9. 
-- This function is so generic, I  didn't even feel the need to use "aap" in the function name.
dropWhileAndCount :: (a -> Bool) -> [a] -> ([a], Int)
dropWhileAndCount _ [] = ([], 0)
dropWhileAndCount p xs@(x:xs') | p x = (fst(nextStep), snd(nextStep)+1)
                               | otherwise = (xs, 0)
                               where nextStep = dropWhileAndCount p xs'

{- Assignment 1 -}

-- Return the number represented by the given array of decimal numbers.
fromDec :: [Int] -> Int
fromDec = fromAap 10

{- Assignment 2 -}

-- Return the array of decimal numbers representing the given number.
toDec :: Int -> [Int]
toDec = toAap 10

{- Assignment 3 -}

-- Return the number represented by the given array of binary numbers.
fromBin :: [Int] -> Int
fromBin = fromAap 2

{- Assignment 4 -}

-- Return the array of binary numbers representing the given number.
toBin :: Int -> [Int]
toBin = toAap 2

{- Assignment 5 -}

-- Return the number represented by the given array of characters, using a given base >=2 and <=36.
-- [0..9] represent themselves, while [a..z] represent [10..35].
fromBase :: Int -> [Char] -> Int
fromBase base c | base >= 2 && base <= 36 = fromAap base (map charToIntAap c)
                | otherwise = error "fromBase: Base must be between 2 and 36 (inclusive)."

{- Assignment 6 -}

-- Return the array of characters, representing a given (decimal) Int in a given base >= 2 and <= 36.
-- See fromBase (assignment 5).
toBase :: Int -> Int -> [Char]
toBase base x | base >= 2 && base <= 36 = map intToCharAap (toAap base x)
              | otherwise = error "toBase: Base must be between 2 and 36 (inclusive)."

{- Assignment 7 -}

-- Return tuples with the given words and the integers they represent in a given base.
-- Output doesn't fully match the assignment. The program throws an exception and terminates as soon as it encounters any illegal input, 
-- such as the character 'z' when using base 10. While this may be a bit excessive, it does have its advantages. At least now you'll be 
-- told when you feed illegal input, arguably better than just ignoring the illegal input. Int, I'm looking at you. Angrily.
-- A fix would involve redefining several functions to use Maybe, returning Nothing for undefined values. Lots of work, since it involves 
-- changing the types of several functions, and rewriting any functions that depend on them. Being able to catch exceptions would be nice...
numbers :: Int -> [String] -> [(String, Int)]
numbers base [] = []
numbers base words = (eerste, getal): (numbers base (tail words))
                   where eerste = head words
                         getal = fromBase base eerste

{- Assignment 8 -}

-- Return a pair of functions that, for a given base, can respectively deconstruct and construct a Gray-code of any natural number.
grayCode :: Int -> ([Char] -> Int, Int -> [Char])
grayCode base | base >=2 && base <= 36 = (grayDecoder base, grayCoder base)
              | otherwise = error "grayCode: Base must be between 2 and 36 (inclusive)."

-- Return a Gray-code of a given number in a given base.
grayCoder :: Int -> Int -> [Char]
grayCoder base number = grayListCoder base (toAap base number) 0
                      where grayListCoder _ [] _ = []
                            grayListCoder base (x:xs) shift = intToCharAap grayDigit: (grayListCoder base xs (shift+grayDigit)) 
                                                            where grayDigit = (x - shift) `mod` base

-- Return the number represented by a given Gray-code in a given base.
grayDecoder :: Int -> [Char] -> Int
grayDecoder base list = fromAap base (grayDecoderAap base list 0)
    where grayDecoderAap _ [] _ = []
          grayDecoderAap base (x:xs) shift = unGrayDigit : (grayDecoderAap base xs unGrayDigit)
                                           where unGrayDigit = ((charToIntAap x + shift)`mod` base)

{- Assignment 9 -}

-- For a given starting number, return the look-and-say sequence.
lookAndSay :: Int -> [String]
lookAndSay = lookAndAap.show

-- For a given string, return the look-and-say sequence.
-- Note that the input does not need to be numerical. For instance, take 3 (lookAndAap "aap") yields ["aap","2a1p","121a111p"].
lookAndAap :: String -> [String]
lookAndAap word = word:(lookAndAap (wordinize word))
                where wordinize [] = [] 
                      wordinize xs@(x:xs') = (show (snd droppinate)) ++ (x: (wordinize (fst droppinate)))
                      -- wordinize: return the next element of the look-and-say sequence containing a given string.
                                           where droppinate = dropWhileAndCount (==x) xs
                                           -- droppinate: return the number of identical characters at the start of the given string, and 
                                           --             the string without these characters.

{- Assignment 10 -}

-- Return a list of Keith numbers.
keithGetallen :: [Integer]
keithGetallen = aapDieKeithHeetGetallen 10

-- Return a list of Keith numbers, starting at a given Integer value.
aapDieKeithHeetGetallen :: Integer -> [Integer]
aapDieKeithHeetGetallen x | isJust keithAap = fromJust keithAap : (aapDieKeithHeetGetallen (x+1))
                          | otherwise = aapDieKeithHeetGetallen (x+1)
                          where keithAap = keithGetal x

-- Return the given Integer wrapped in Just, if said Integer is a Keith number. Otherwise, return nothing.
keithGetal :: Integer -> Maybe Integer
keithGetal x | isKeithGetalEfficienter x = Just x
             | otherwise = Nothing

-- Check whether a given Integer is a Keith number.
isKeithGetal :: Integer -> Bool
isKeithGetal x = isKeithLijst (toAap 10 x) x

-- Check whether a given Integer is a Keith number, using the alternative algorithm from isKeithLijstAccum
isKeithGetalEfficienter :: Integer -> Bool
isKeithGetalEfficienter x = isKeithLijstAccum (aaplijst++[sum aaplijst]) (sum aaplijst) x
                          where aaplijst = (toAap 10 x)

-- Generate the sequence of numbers required to check whether the start value is a Keith number, then return true if it is, false otherwise.
isKeithLijst :: [Integer] -> Integer -> Bool
isKeithLijst xs x | keithSom >= x = keithSom == x
                  | otherwise = isKeithLijst ((drop 1 xs) ++ [keithSom]) x
                  where keithSom = sum xs

-- Construct the Keith sequence in a possibly more efficient way.
-- Based on the following property:
--  keithSom = sum (x:xs)
--  next keithSom = sum (xs ++ [keithSom]) = sum xs + keithSom
--  thus next keithSom = 2 * keithSom - x
-- While in theory this should save the effort of calculating the entire sum for every element, I'm sceptical about the actual effect on runtime.
-- GHC may or may not be lazy enough to recognize the situation. Also, I've no idea to what extent cached results may influence runtime.
-- Note: Rough tests using GHCi's :set +s option indicate isKeithGetal to have a slightly lower runtime than isKeithGetalEfficienter, while 
-- isKeithGetalEfficienter uses somewhat less memory. I have an itchy feeling a much more efficient algorithm exists.
isKeithLijstAccum :: [Integer] -> Integer -> Integer -> Bool
isKeithLijstAccum (x:xs) prev start | keithSom >= start = keithSom == start
                                    | otherwise = isKeithLijstAccum (xs++[keithSom]) keithSom start
                                    where keithSom = 2 * prev - x