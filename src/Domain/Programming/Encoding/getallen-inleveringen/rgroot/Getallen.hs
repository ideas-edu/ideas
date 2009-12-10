----Practicum 1 Getallen
--naam: Roy de Groot 
--studentnummer: 3238679
--compiler: GHC 6.10.1

module Getallen where

import Data.List
import Data.Char
import Data.Maybe

------Getallensystemen:
----Algemene Functie Opdracht 1 & 3
--Produceert voor een grontal 2-10 en zijn losse getallen in een lijst zijn
--Int-getal.
fromIntGet :: Integral a => a -> [a] -> a
fromIntGet _ [] = 0
fromIntGet g (x:xs) =  g^(length xs) * x + fromIntGet g xs

----Algemene Functie Opdracht 2 & 4
--Produceert voor grondtal 2-10 en een Int-getal een lijst losse getallen waarbij 
--de losse getallen in de lijst dezelfde volgorde hebben als het Int-getal. 
toIntGet :: Integral a => a -> a -> [a]
toIntGet g x = reverse (toIntGet2 g x)

--Produceert voor grondtal 2-10 en een Int-getal een lijst losse getallen waarbij 
--de losse getallen in de lijst achterste-voren staan i.v.m. het Int-getal.
toIntGet2 :: Integral a => a -> a -> [a]
toIntGet2 g x | x < g = [x]
              | otherwise = (x `mod` g) : toIntGet2 g (x `div` g)
              
----Opdracht 1
--Doormiddel van de algemene functie aan te roepen met grondtal 10 wordt van 
--een lijst van Int-getallen het bijhorden Int-getal berekend.
fromDec :: [Int] -> Int
fromDec x = fromIntGet 10 x 
              
----Opdracht 2
--Doormiddel van de algemene functie aan te roepen met grondtal 10 wordt van 
--Int-getal de bijhorende lijst van losse Int-getallen berekend.
toDec :: Int -> [Int]
toDec x = toIntGet 10 x

----Opdracht 3
--Doormiddel van de algemene functie aan te roepen met grondtal 2 wordt van 
--een lijst van Int-getallen het bijhorden Int-getal berekend.
fromBin :: [Int] -> Int
fromBin x = fromIntGet 2 x

----Opdracht 4
--Doormiddel van de algemene functie aan te roepen met grondtal 2 wordt van 
--Int-getal de bijhorende lijst van losse Int-getallen berekend.
toBin :: Int -> [Int]
toBin x = toIntGet 2 x
		 
--Opdracht 5
--Produceert gegeven een grontal en zijn n-aire Code het bijhorende Int-getal.  
fromBase :: Int -> [Char] -> Int
fromBase _ [] = 0
fromBase g (x:xs) | validBase g(x:xs) = g^(length (x:xs)-1) * r + fromBase g xs
                  | otherwise = error "Illegale Code bij dit grondtal"   
                                        where r = toInt2 (toLower x)

--Een hulpfunctie die gegeven een grontal de een n-aire Code checkt 
--of de n-aire Code legaal is bij dit grondtal.
validBase :: Int -> [Char] -> Bool
validBase _ [] = True
validBase g (x:xs) = g >= toInt2 x && validBase g xs

--Hulpfucntie die een Char '0' - 'z' converteert naar zijn Int-waarde
toInt2 :: Char -> Int
toInt2 x | isHexDigit x = digitToInt x 
         | otherwise = (ord x - ord 'a') + 10
		 
----Opdracht 6
--Zet gegeven een grondtal en een Int-getal deze om naar een lijst van bijhorende
--characters.
toBase :: Int -> Int -> [Char]
toBase g x = reverse (toBase2 g x)

--Zet gegeven een grondtal en een Int-getal deze om naar een omgekeerde lijst
--van characters.
toBase2 :: Int -> Int -> [Char]
toBase2 g x | x < g = [r]
            | otherwise = toChar2 (x `mod` g) : toBase2 g (x `div` g)
              where r = toChar2 x

--Hulpfunctie die een Int-getal converteert naar zijn bijhorende character.
toChar2 :: Int -> Char
toChar2 x | x < 16 = intToDigit x
          | otherwise = chr (x + ord 'a' - 10)
          
----Opdracht 7
--Geeft gegeven een grondtal en een lijst van Strings een lijst van tupels 
--terug met de String zelf en de omgezette waarde van Int. Een tupel wordt 
--alleen in het eindresultaat opgenomen als bij het grondtal een legale String 
-- gedefinieerd is volgens de eerdere hulpfunctie validBase 
numbers :: Int -> [String] -> [(String,Int)]
numbers _ [] = []
numbers g (x:xs) | validBase g x = (x, fromBase g x) : numbers g xs
                 | otherwise = numbers g xs

------Gray-coderingen
----Opdracht 8
--A.h.v. het grondtal wordt een tupel van de decode- en encode-functie voor 
--Gray-code opgeleverd.
grayCode :: Int -> ([Char] -> Int, Int -> [Char])
grayCode g = (fromGray g, toGray g)

-----------testG x xs = map (fromGray x) (map (toGray x) xs)

--Bepaalt met het grondtal en de Gray-code het bijhorende Int-getal
fromGray :: Int -> [Char] -> Int
fromGray g (x:xs) | even g = fromBase g (fromGrayE g x xs)
                  | otherwise = fromBase g (fromGrayO g "" (x:xs))

--Bepaalt a.h.v. even grondtal en Gray-code de grondtal-naire code produceert
fromGrayE :: Int -> Char -> [Char] -> [Char]
fromGrayE _ x [] = [x]
fromGrayE g x (y:ys) | odd (toInt2 x) = x : fromGrayE g x2 ys
                     | otherwise = x : fromGrayE g y ys
                          where x2 = toChar2 (g-1 - toInt2 y)

--Bepaalt a.h.v. oneven grondtal en GrayCode de grondtal-naire code produceert    
fromGrayO :: Int -> [Char] -> [Char] -> [Char]
fromGrayO _ _ [] = []
fromGrayO g x (y:ys) | odd (sum2 x) = x2 : fromGrayO g (x2:x) ys
                     | otherwise = y : fromGrayO g (y:x) ys
                               where x2 = toChar2 (g-1 - toInt2 y)

--Bepaalt a.h.v. het grondtal en Int-getal de bijhorende Gray-code.
toGray :: Int -> Int -> [Char]
toGray g x | x < 0 = error "Kan niet!" 
           | even g = reverse (toGrayE g b)
           | otherwise = reverse (toGrayO g b)
                where b = toBase2 g x
  
--Bepaalt a.h.v. een even grondtal en Int-getal de bijhorende Gray-code.
toGrayE :: Int -> [Char] -> [Char]
toGrayE _  [x] = [x]
toGrayE g (x:y:ys) | odd (toInt2 y)  = toChar2 (g-1 - toInt2 x) : xs
                   | otherwise = x : xs
                        where xs = toGrayE g (y:ys)

--Bepaalt a.h.v. een oneven grondtal en Int-getal de bijhorende Gray-code.
toGrayO :: Int -> [Char] -> [Char]
toGrayO _ [x] = [x]
toGrayO g (x:y:ys) | odd (sum2 (y:ys)) = toChar2 (g-1 - toInt2 x) : xs
                   | otherwise = x : xs
                        where  xs  = toGrayO g (y:ys)                        

--Hulpfunctie om een lijst van Chars op te tellen
sum2 :: [Char] -> Int
sum2 x = sum (map toInt2 x)

------Look-and-say-reeksen
----Opdracht 9
--Produceert aan de hand van een Int-getal zijn oneindige look-and-say-reeks.
lookAndSay :: Int -> [String]
lookAndSay x = lookAndSay2 ([show x])

--Produceert ook een look-and-say-reeks maar dan a.h.v. een lijst van String.
lookAndSay2 :: [String] -> [String]
lookAndSay2 (x:xs) =  x: lookAndSay2 [lookAndSay3 x]

--Produceert voor een gegeven String zijn opvolger in de look-and-say-reeks.
lookAndSay3 :: String -> String
lookAndSay3 [] = []
lookAndSay3 (x:xs) = toChar2 (length (fst k)): x: (lookAndSay3 (snd k))
                      where k = span (== x) (x:xs)

------Keith-getallen
----Opdracht 10
--Produceert ene oneindige lijst van de Keithgetallen, waarbij 
--getallen < 10 niet worden meegenomen.
keithGetallen :: Integral a => [a]
keithGetallen = filter keith [10..]

--Kijkt gegeven een natuurgetlijk getal of dit een Keitgetal is
--en levert een Boolean op of het een Keitgetal is.
keith :: Integral a => a -> Bool
keith x = elem  x (takeWhile (<= x) k)
          where k = keithList (toIntGet 10 x)

--Creeërt aan de hand van een lijst van een natuurlijk getal zijn oneindige lijst
--d.m.v. het Keith principe toe te passen.
keithList :: Integral a => [a] -> [a]
keithList (x:xs) = x : keithList (xs ++ [sum (x:xs)])

--Einde    