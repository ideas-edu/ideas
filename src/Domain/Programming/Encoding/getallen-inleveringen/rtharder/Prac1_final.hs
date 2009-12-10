--
-- Functioneel Programmeren
-- Practicum 1
-- Rutger Harder (rtharder)
-- 3220508
-- 02/03/2009
--

import Char

-- Hulpfuncties
--   Deze hulpfuncties worden gebruikt om
--   integers om te zetten naar characters
--   en andersom

fromDigit :: Char -> Int
fromDigit x | isDigit x = fromEnum x - 48
            | otherwise  = fromEnum (toLower x) - 87

toDigit :: Int -> Char
toDigit x | x < 10    = toEnum (x+48)::Char
          | otherwise = toEnum (x+87)::Char

-- Opgave 1
fromDec :: [Int] -> Int
fromDec [] = 0
fromDec (x:xs) = x*10^(length xs) + fromDec xs 

-- Opgave 2
toDec :: Int -> [Int]
toDec 0 = []
toDec a = toDec ((a-x) `div` 10) ++ [x]
    where x = a `mod` 10

-- Opgave 3
fromBin :: [Int] -> Int
fromBin [] = 0
fromBin (x:xs) = x*2^(length xs) + fromBin xs

-- Opgave 4
toBin :: Int -> [Int]
toBin 0 = []
toBin a = toBin ((a-x) `div` 2) ++ [x]
    where x = a `mod` 2
    
-- Opgave 5
fromBase :: Int -> [Char] -> Int
fromBase _ [] = 0
fromBase n (ux:xs) = (fromDigit ux)*n^(length xs) + fromBase n xs
    
-- Opgave 6
toBase :: Int -> Int -> [Char]
toBase _ 0 = []
toBase n a = toBase n ((a-ux) `div` n) ++ [toDigit ux]
    where ux = a `mod` n

-- Opgave 7
--   de checkStr functie checkt of een woord om te zetten
--   is naar een getal door van elk afzonderlijk karakter
--   te tellen of het binnen de base valt
checkStr :: Int -> [Char] -> Bool
checkStr _ [] = True
checkStr n (x:xs) = (fromBase n [x] < n) && (checkStr n xs)

numbers :: Int -> [String] -> [(String,Int)]
numbers _ [] = []
numbers n (x:xs) | checkStr n x = (x,fromBase n x) : numbers n xs
                 | otherwise    = numbers n xs

-- Opgave 8
--  Deze Gray functie werkt door het afwisselend op- en aflopen van
--  de getallen, afhankelijk van of het meer significante getal
--  even of oneven is. Je krijgt dus deze lijst bij een base van 10
--    1 2 3 4 5 6 7 8 9 19 18 17 16 15 14 13 12 11 10 20 21 22 enz..

grayCode :: Int -> ([Char]->Int,Int->[Char])
grayCode n = (fromGray n, toGray n)

fromGray :: Int -> [Char] -> Int
fromGray n a = fromBase n (gray n 0 a)

toGray :: Int -> Int -> [Char]
toGray n a = gray n 0 (toBase n a)

gray :: Int -> Int -> [Char] -> [Char]
gray _ _ [] = []
gray n c (x:xs) | even c    = x : gray n (fromDigit x) xs
                | otherwise = toDigit (n - (fromDigit x + 1)) : gray n (fromDigit x) xs

-- Opgave 9
--   De hulpfunctie parseInt neemt een string van digits, telt hoevaak het eerste
--   element voorkomt, en plakt dat aan de lijst, waarna de functie weer recursief
--   wordt aangeroepen

parseInt :: String -> String
parseInt [] = []
parseInt (x:xs) = toDigit (length (takeWhile (==x) (x:xs))) : x : parseInt (dropWhile (==x) (x:xs))

lookAndSay :: Int -> [String]
lookAndSay n = toBase 10 n : lookAndSay' n

lookAndSay' :: Int -> [String]
lookAndSay' n = p : lookAndSay' (fromBase 10 p)
  where a = toBase 10 n
        p = parseInt a
        
-- Opgave 10
--   De keithGetallen worden gegenereerd door simpelweg een oneindige
--   lijst van lijsten te genereren, en van elk van die lijsten te
--   checken of het keith getal er in voorkomt.
--   Het genereren van zo'n lijst gebeurt door het telkens meegeven
--   van de lijst die bij elkaar opgetelt moet worden

keithGetallen :: [Int]
keithGetallen = [ a | a <- [1..], a > 9, checkKeith a (listGen a)]

checkKeith :: Int -> [Int] -> Bool
checkKeith _ [] = False
checkKeith a (x:xs) = ( a == x ) || ( ( a > x ) && (checkKeith a xs) )

listGen :: Int -> [Int]
listGen a = b ++ listGen' b
  where b = map fromDigit (toBase 10 a)

listGen' :: [Int] -> [Int]
listGen' [] = []
listGen' (x:xs) =  c : listGen' (xs++[c])
  where c = sum (x:xs)