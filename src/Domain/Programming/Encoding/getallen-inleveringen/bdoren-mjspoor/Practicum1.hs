{-- Practicum 1 --}
-----------------------------------------------------------

{-- Deelnemers --}
-- Bas van Doren, bdoren, 3401898
-- Marten Spoor, mjspoor, 3344940

{-- Compiler --}
-- GHC
-----------------------------------------------------------

module Practicum1 where

import Char

{-- Opgave 1 --}
-----------------------------------------------------------
fromDec :: [Int] -> Int

fromDec = foldl (\x y -> x * 10 + y) 0
-- het toepassen met recursie van 'x * 10 (want decimaal) + y' op de getalrepresentatie (lijst) geeft het geconverteerde getal

{-- Opgave 2 --}
-----------------------------------------------------------
toDec :: Int -> [Int]

toDec 0 = []
toDec num = toDec (num `div` 10) ++ [num `rem` 10]

{-- Opgave 3 --}
-----------------------------------------------------------
fromBin :: [Int] -> Int

fromBin = foldl (\x y -> x * 2 + y) 0
-- het toepassen met recursie van 'x * 2 (want binair) + y' op de getalrepresentatie (lijst) geeft het geconverteerde getal

{-- Opgave 4 --}
-----------------------------------------------------------
toBin :: Int -> [Int]

toBin 0 = []
toBin num = toBin (num `div` 2) ++ [num `rem` 2]

{-- Opgave 5 --}
-----------------------------------------------------------
digitValue :: Char -> Maybe Int

digitValue c
    | asc c >= ord '0' && asc c <= ord '9' = Just (asc c - ord '0')
    | asc c >= ord 'A' && asc c <= ord 'Z' = Just (asc c - (ord 'A' - 10))
    | otherwise = Nothing
    where
    asc = ord.toUpper
-- zoekt de waarde die bij het karakter hoor en vindt die, misschien

orMaybe :: a -> Maybe a -> a

orMaybe _ (Just b) = b
orMaybe a Nothing = a
-- geeft een alternatief voor als een Maybe waarde Nothing is

-- Opgave
fromBase :: Int -> [Char] -> Int

fromBase radix = (foldl (\x y -> x * radix + y) 0).map ((0 `orMaybe`).digitValue)
-- het toepassen met recursie van 'x * radix + y' op de naar waarde geconverteerde digits geeft het geconverteerde getal

{-- Opgave 6 --}
-----------------------------------------------------------
valueDigit :: Int -> Char

valueDigit i
    | i >= 0 && i <= 9 = chr (i + ord '0')
    | i >= 10 && i <= 36 = chr (i - 10 + ord 'A')
    | otherwise = '0'
-- zoek een geschikt character voor een gegeven waarde

-- Opgave
toBase :: Int -> Int -> [Char]

toBase _ 0 = []
toBase radix num = toBase radix (num `div` radix) ++ [valueDigit (num `rem` radix)]
-- pak de eenheden (rest-bij-deling door grondtal) en zoek het karakter voor dit getal, plak met recursie de rest van het getal ervoor

{-- Opgave 7 --}
-----------------------------------------------------------
numbers :: Int -> [String] -> [(String, Int)]

numbers radix ns = [(a, fromBase radix a) | a <- ns, inRadix a]
-- bouwt een lijst van tuples bestaande uit Strings die binnen de genoemde radix vallen en de bijbehorende waarde
   where
   inRadix [] = True -- niks (meer) te controleren, geen False tegengekomen, dus True is het antwoord
   inRadix (c : str)
        | 99 `orMaybe` digitValue c < radix = inRadix str -- dit karakter was goed (een digit kleiner dan het grondtal), dus we gaan verder
        | otherwise = False -- dit karakter was fout en daarmee is de String dus geen getal

{-- Opgave 8 --}
-----------------------------------------------------------
-- TBA

{-- Opgave 9 --}
-----------------------------------------------------------
lookAndSay :: Int -> [String]

lookAndSay = (iterate (runlength 1)).toBase 10
    where
    runlength _ [] = []
    runlength i (c : str)
        | str == [] || c /= head str = (toBase 10 i) ++ (c : (runlength 1 str))
        -- het huidige teken is gelijk aan het volgende teken of er zijn geen tekens meer, dus we voeren de teller en het bijbehorende karakter uit
        | otherwise = runlength (i + 1) str
        -- niks nieuws onder de zon, dus zoeken en tellen we verder

{-- Opgave 10 --}
-----------------------------------------------------------
toBigDec :: Integer -> [Integer]

toBigDec 0 = []
toBigDec num = toBigDec (num `div` 10) ++ [num `rem` 10]
-- toDec voor grote getallen

recursieKG :: [Integer] -> [Integer]

recursieKG []     = []
recursieKG (x:xs) = (xs) ++ [sum(x:xs)]
-- berekent het volgende element van de lijst, en gooit de eerste weg

checkKG :: Integer -> [Integer] -> Bool

checkKG 0 (x:xs)                  = False
checkKG p []                      = False
checkKG p (x:xs) | p > last(x:xs) = checkKG (p) (recursieKG(x:xs))
                 | p < last(x:xs) = False
                 | p ==last(x:xs) = True
-- controleert met behulp van invoer van decimale waarde en allereerste lijst waarde of het een keithgetal is

keithGetallen :: [Integer]

keithGetallen =  filter (\num -> checkKG num (toBigDec num)) [10..]
