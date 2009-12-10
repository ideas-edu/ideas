-- FP Practicum 1
--Rik Vermeulen 3034372
--Jonathan Vrijhof 3117936

import Char

-- Opgave 1
fromDec :: [Int] -> Int
fromDec [] = 0
fromDec (c:cs) = c * (10 ^ length cs) + fromDec cs

-- Opgave 2
toDec  :: Int -> [Int]
toDec2 :: Int -> [Int]

toDec a = reverse (toDec2 a)

toDec2 0 = []
toDec2 a =  b : toDec2 ((a - b) `div` 10) where b = a `mod` 10

-- Opgave 3
fromBin :: [Int] -> Int
fromBin [] = 0
fromBin (c:cs) = c * (2 ^ length cs) + fromBin cs

-- Opgave 4
toBin  :: Int -> [Int]
toBin2 :: Int -> [Int]

toBin a = reverse (toBin2 a)

toBin2 0 = []
toBin2 a =  b : toBin2 ((a - b) `div` 2) where b = a `mod` 2

-- Opgave 5
fromBase :: Int -> [Char] -> Int

fromBase _ [] = 0
fromBase a (c:cs) = if ((charToInt c) > a)
                    then  error "Character heeft te hoge waarde voor het grondtal."
                    else charToInt c * (a ^ length cs) + fromBase a cs

-- Hulp-functie die de corresponderende integer geeft aan de hand van de  ASCII-code van het character
charToInt :: Char -> Int
charToInt char = if c > 96 && c < 123
                 then c - 87
                 else if c > 47 && c < 58 then c - 48 else error "ongeldig character."
                 where c = ord char

-- Hulp-functie die de corresponderende character geeft aan de hand van zijn ASCII-code
intToChar :: Int -> Char
intToChar int =  if int < 10
                 then chr (int + 48)
                 else if int < 36 then chr (int + 87) else error "Getal te hoog."

-- Opgave 6
toBase :: Int -> Int -> [Char]
toBase _ 0  = []
toBase grond x  = toBase grond ((x - c) `div` grond) ++ [b] where c = x `mod` grond
                                                                  b = intToChar c

-- Opgave 7
numbers :: Int -> [String] -> [(String, Int)]
numbers _ [] = []
numbers grond (x:xs) = if grondCheck grond x == False then numbers grond xs else (x, fromBase grond x): numbers grond xs

-- Hulp-functie die checkt of een String geldig is voor het ingevoerde grondtal
grondCheck :: Int -> [Char] -> Bool
grondCheck _ [] = True
grondCheck grond (x:xs) = if ( charToInt x ) > grond then False else grondCheck grond xs

-- Opgave 8
{-grayCode :: Int -> ([Char] -> Int, Int -> [Char])
grayCode grond-}

-- Opgave 9
lookAndSay :: Int -> [String]
lookAndSay int = intToString int : (lookAndSay (stringToInt (getInts (intToString int) 1)))

-- Hulp-functies die van een String de bijbehorende integer maken met behulp van de lengte van de String
stringToInt :: String -> Int
stringToInt string = stringToInt2 string (length string)

stringToInt2 :: String -> Int -> Int
stringToInt2 [] _ = 0
stringToInt2 (x:xs) l = (charToInt x * (10 ^ (l - 1))) + stringToInt2 xs (l - 1)

-- Hulp-functie die van een int de bijbehorende String maakt
intToString :: Int -> String
intToString x = (show x)

-- Hulp-functie die m.b.v een accumulator een String omzet naar een opsomming van de getallen die hij bevat
getInts :: String -> Int -> String
getInts (x:[])   acc = (intToString acc) ++ [x]
getInts (x:y:[]) acc = if x /= y then (intToString acc) ++ [x] ++ intToString 1 ++ [y] else (intToString (acc + 1)) ++ [x]
getInts (x:y:xs) acc = if x /= y then (intToString acc) ++ [x] ++ getInts (y:xs) 1 else getInts (y:xs) (acc + 1)

-- Opgave 10
maakKeithReeks :: Int -> [Int]
maakKeithReeks int = toDec int ++ [telInt int] ++ tail (tail ( maakKeithReeks (stringToInt (tail (intToString int) ++ intToString (telInt int)))))

-- bepaalt of het gegeven getal n een Keith-getal is (deze functie neemt de eerste n getallen van de reeks)
keithGetal :: Int -> Bool
keithGetal int = vindElem (take int (maakKeithReeks int)) int

-- Hulp-functie die kijkt of een element voorkomt in een lijst
vindElem :: [Int] -> Int -> Bool
vindElem [] _ = False
vindElem (x:xs) int = if x == int then True else vindElem xs int

-- Hulp-functies die een integer uit elkaar halen en de aparte cijfers bij elkaar optelt
telInt :: Int -> Int
telInt int = telInt2 int (length (intToString int))

telInt2 :: Int -> Int -> Int
telInt2 _ 0 = 0
telInt2 int l = int `mod` 10 + telInt2 ((int - (int `mod` 10)) `div` 10) (l - 1)