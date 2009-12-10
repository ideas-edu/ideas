module Getallen where

{-----------------------------------------------------------
Teamleden: Selene Broers (3117243) en Jasper Klein (3403602)
Compiler: GHC (Dus niet Helium!)
-----------------------------------------------------------}

--	Opgave 1

--  Slaat een lijst van Int plat tot één geheel decimaal getal

fromDec :: [Int] -> Int
fromDec l = foldl (\a b -> a * 10 + b) 0 l

--	Opgave 2

--  Maakt van een decimaal getal een lijst waar zijn onderdelen 1 voor 1 in staan

toDec :: Int -> [Int]
toDec = reverse . toDec'

toDec' :: Int -> [Int]
toDec' 0 = []
toDec' x = rem x 10 : toDec' (quot x 10)

--	Opgave 3

--  Converteert een binair getal naar een decimaal getal.

fromBin :: [Int] -> Int
fromBin x = fromBin' x (length x - 1)

fromBin' :: [Int] -> Int -> Int
fromBin' [] _ = 0
fromBin' (x:xs) y = x*2^y + (fromBin' xs (y - 1))

--	Opgave 4

--  Converteert een decimaal getal naar een binair getal.

toBin :: Int -> [Int]
toBin = reverse . toBin'

toBin' :: Int -> [Int] 
toBin' 0 = []
toBin' x = mod x 2 : toBin' (div x 2)

--	Opgave 5

--  Geeft voor een getal met grondtal tussen 2 en 36 de bijbehorende int. 

fromBase :: Int -> String -> Int
fromBase x l = foldl (\a b -> a * x + b) 0 (map naarGetal l)

naarGetal :: Char -> Int
naarGetal x | x >= '0' && x <= '9' = fromEnum x - 48
            | x >= 'A' && x <= 'Z' = fromEnum x - 55
            | x >= 'a' && x <= 'z' = fromEnum x - 87

--	Opgave 6

--  Geeft voor een grondtal tussen 2 en 36 de bijbehorende cijferreeks.

toBase :: Int -> Int -> String
toBase x = reverse . toBase' x

toBase' :: Int -> Int -> String
toBase' _ 0 = []
toBase' g x = vanGetal (mod x g) : toBase' g (div x g)

--  Alleen getallen van 0 t/m 35
vanGetal :: Int -> Char
vanGetal x | x >= 0 && x <= 9 = toEnum (x + 48) :: Char
		   | x >= 10 && x <= 35 = toEnum (x + 87) :: Char
		   | otherwise = error "Je hebt me een getal gegeven dat niet tussen -1 en 36 ligt!"

--	Opgave 7

--  Geeft voor een grondtal tussen 2 en 36 en een lijst met woorden de woorden die overeenkomen met een getal

numbers :: Int -> [String] -> [(String, Int)]
numbers _ [] = []
numbers x (t:ts) | allesIsKleiner x t = (t, (fromBase x t)) : numbers x ts
                 | otherwise = numbers x ts

allesIsKleiner :: Int -> String -> Bool
allesIsKleiner x t = all (<=x) (map naarGetal t)

--	Opgave 8

--  Klopt nog niet, maar niet werkend gemaakt ivm een veranderde opgave die wij te laat hebben gezien. Zie bijgevoegde pdf voor de oude.
grayCode :: Int -> (String -> Int, Int -> String)
grayCode b = (fgc, tgc)

fgc :: String -> Int
fgc x = fromBin (map naarGetal (fgc' x))

fgc' :: String -> String
fgc' [x] = [x]
fgc' (x:y:xs) | x == '0' = x : fgc' (y:xs)
              | x == '1' && y == '0' = x : fgc' ('1' : xs) 
              | x == '1' && y == '1' = x : fgc' ('0' : xs)

tgc :: Int -> String
tgc 0 = "0"
tgc a = map vanGetal (head (toBin a) : tgc' (toBin a))

tgc' :: [Int] -> [Int]
tgc' (x:y:xs) | x == 0 = y : tgc' (y:xs)
              | x == 1 && y == 0 = 1 : tgc' (y:xs)
              | x == 1 && y == 1 = 0 : tgc' (y:xs)
tgc' (x:y) = y 

--	Opgave 9 

--  Geeft een Conway-reeks voor een gegeven getal.

lookAndSay :: Int -> [String]
lookAndSay n = f : lookAndSay'' f
			   where f = show n

lookAndSay'' :: String -> [String]
lookAndSay'' s = f : lookAndSay'' f
				 where f = lookAndSay' s

lookAndSay' :: String -> String
lookAndSay' [] = []
lookAndSay' n = show (telZelfde n) ++ (head n) : lookAndSay' (drop (telZelfde n) n)

--	Telt hoeveel dezelfde getallen er achter elkaar staan en laat de rest van de string onaangeroerd
telZelfde :: Eq a => [a] -> Int
telZelfde [] = 0
telZelfde [x] = 1
telZelfde (x:xs) | x == head xs = 1+ (telZelfde xs)
                 | x /= head xs = 1

--  Opgave 10

--  Geeft de lijst van Keith-getallen vanaf 14.

keithGetallen :: [Int]
keithGetallen = lijstKeith 14

--  Roept isKeith aan met steeds 1 getal hoger.
lijstKeith :: Int -> [Int]
lijstKeith x | isKeith x = x : lijstKeith (x + 1)
             | otherwise = lijstKeith (x + 1)

--  Controleert of een getal een Keith-getal is.
isKeith :: Int -> Bool
isKeith x = x == keithSequence x (toDec x)

--  Loopt de reeks af totdat het getal groter of gelijk is aan het getal waarop deze moet testen. Is de som van de lijst gelijk aan het getal x dan true en als deze groter is dan false. 
keithSequence :: Int -> [Int] -> Int
keithSequence x (a:as) = if (last as >= x) then last as
                         else keithSequence x (as ++ (sum (a:as)) : [])