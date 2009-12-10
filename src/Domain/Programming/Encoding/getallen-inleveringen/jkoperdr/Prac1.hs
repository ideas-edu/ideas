module Prac1 where
--
import Data.Char
--
-- Omdat we veel met delingen en resten daarvan te maken krijgen is de modulo
-- functie nodig. Deze is niet als % gedefinieerd in de prelude, dus definieer
-- ik die hier zelf:
--
-- Hulpfunctie modulo, %
(%) :: Int -> Int -> Int
(%) x y = x - y * (x `div` y)
--
-- Opgave 1 - fromDec
--
fromDec :: [Int] -> Int
fromDec []     = 0
fromDec (x:xs) = x * 10 ^ n + fromDec xs
		where n = length xs
--
-- Opgave 2 - toDec
--
--
-- toDec
toDec :: Int -> [Int]
toDec n | n < 0     = error ("De functie toDec is niet gedefinieerd"
                          ++ " voor getallen < 0.")
        | n < 10    = [n]
        | otherwise = toDec (n `div` 10) ++ [n % 10]
--
-- Opgave 3 - fromBin
--
fromBin :: [Int] -> Int
fromBin [] = 0
fromBin (x:xs) = x * 2 ^ n + fromBin xs
		where n = length xs
--
-- Opgave 4 - toBin
--
toBin :: Int -> [Int]
toBin n | n < 0     = error ("De functie toBin is niet " 
                          ++ "gedefinieerd voor getallen < 0.")
        | n < 2     = [n]
        | otherwise = toBin (n `div` 2) ++ [n % 2]
--
-- Een hulpfunctie om van een char zijn overeenkmostige grondgetal waarde te 
-- bepalen. Deze functie mapt de waardes 0 t/m 9 en a t/m z naar de integers
-- 0 t/m 35
-- Deze functie maakt gebruik van de functie ord uit de module Data.Char
-- Deze module wordt aan het begin van deze module geimporteerd.
--
baseVal :: Char -> Int
baseVal c | o >= 48 && o <= 57  = o - 48      -- 1 t/m 9
          | o >= 97 && o <= 122 = o - 97 + 10 -- a t/m z
          | otherwise           = error ("De functie baseVal is niet "
                                      ++ "gedefinieerd voor characters anders "
                                      ++ "dan 1 t/m 9 of a t/m z.")
	where o = ord c
--
-- Evenzo moeten we terug kunnen van een int naar 1 t/m 9 en a t/m z:
--
baseRep :: Int -> Char
baseRep i | i >= 0 && i <= 9 = chr (i + 48)   -- 1 t/m 9
				  | i >= 10 && i <= 35 = chr (i + 97 - 10) -- a t/m z
				  | otherwise          = error ("De functie baseRep is niet "
				                             ++ "gedefinieerd voor integers < 0 of "
				                             ++ "> 35.")
--
-- Opgave 5 - fromBase
--
fromBase :: Int -> [Char] -> Int
fromBase _ []     = 0
fromBase b (x:xs) = baseVal x * b ^ n + fromBase b xs
		where n = length xs
-- 
-- Opgave 6 - toBase
--
toBase :: Int -> Int -> [Char]
toBase b n | n < 0     = error ("Deze functie is niet gedifinieerd "
                             ++ "voor getallen < 0.")
           | n < b     = [baseRep n]
           | otherwise = toBase b (n `div` b) ++ [baseRep (n % b)]
--
-- Opgave 7 - Numbers
--
numbers :: Int -> [String] -> [(String, Int)]
numbers base = filter f . map m
  where f (_, i) = i >= 0
        m word   | highestChar < base = (word, fromBase base word)
                 | otherwise = ("",(-1))
		where highestChar = foldr max 0 (map baseVal word)
-- 
-- Opgave 8 - grayCode
--
grayCode :: Int -> ([Char] -> Int, Int -> [Char])
grayCode n = (fromBase n, toBase n)
--
-- Opgave 9 -- lookAndSay
--
lookAndSay :: Int -> [String]
lookAndSay = f . show
	where f c = c : f (describe c)
--
describe :: String -> String
describe []     = ""
describe (x:xs) = show (length match + 1) ++ [x] ++ describe rest
  where match = takeWhile ((==) x) xs
        rest  = dropWhile ((==) x) xs
--
-- Opgave 10 -- keithGetallen
--
keithGetallen :: [Int]
keithGetallen = filter f [10..]
  where f i = i `elem` (takeWhile (<= i) (keithReeks i))
--
keithReeks :: Int -> [Int]
keithReeks i = list ++ keith_ list
  where list = toDec i
        keith_ l = som : keith_ (tail l ++ [som])
          where som = foldr (+) 0 l