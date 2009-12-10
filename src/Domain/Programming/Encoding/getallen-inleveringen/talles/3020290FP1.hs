module Getallen where

------------------------------------------
------------------------------------------
-- Functioneel Programmeren Practicum 1 --
--   gemaakt door Tos Alles (3020290)   --
--     gecompileerd in GHCi 06.10.1     --
------------------------------------------
------------------------------------------


-----------------
--  Opgave  1  --
-----------------

fromDec :: [Int] -> Int
-- Het basisgeval, bij één getal kan hij gewoon gereturned worden
fromDec [a] = a
{- In de recursie moet telkens het voorste getal met een tienmacht
   tot de lengte van de dan huidige lijst vermenigvuldigd worden -}
fromDec (a:as) = a * (10 ^ (length (a:as) - 1)) + fromDec(as)

-----------------
--  Opgave  2  --
-----------------

toDec :: Int -> [Int]
-- Draai de lijst om, omdat 'ie anders in de verkeerde volgorde staat
toDec x = reverse (toDec' x)
  where
  	-- Het basisgeval
    toDec' 0 = []
    -- Deel het getal door 10, zet de rest in de lijst en ga door met uitkomst
    toDec' y = let (a,b) = quotRem y 10 in [b] ++ toDec' a

-----------------
--  Opgave  3  --
-----------------

fromBin :: [Int] -> Int
-- Het basisgeval
fromBin [a] = a
-- Pak het eerste cijfer uit de lijst en vermenigvuldig dat met 2 tot de lengte
-- van de lijst min 1 en ga door met de rest van de lijst
fromBin (a:as) = a * (2 ^ (length (a:as) - 1)) + fromBin(as)

-----------------
--  Opgave  4  --
-----------------

toBin :: Int -> [Int]
-- Draai de lijst om, omdat 'ie anders in de verkeerde volgorde staat
toBin x = reverse (toBin' x)
  where
    -- Het basisgeval
    toBin' 0 = []
    -- Deel het getal door 2, zet de rest in de lijst en ga door met de uitkomst
    toBin' y = let (a,b) = quotRem y 2 in [b] ++ toBin' a

-----------------
--  Opgave  5  --
-----------------

{- Werkt alleen met getallen -}
-- Het lukte mij niet de cijfers in getallen om te zetten
fromBase :: Int -> [Int] -> Int
-- Het basisgeval
fromBase b [a] = a
-- Pak het eerste cijfer uit de lijst en vermenigvuldig dat met het grondtal
-- tot de lengte van de lijst min 1 en ga door met de rest van de lijst.
fromBase b (a:as) = a * (b ^ (length (a:as) - 1)) + fromBase b as

-----------------
--  Opgave  6  --
-----------------

{- Werkt alleen met getallen -}
-- Het lukte mij niet de cijfers in getallen om te zetten
toBase :: Int -> Int -> [Int]
-- Draai de lijst om, omdat 'ie anders in de verkeerde volgorde staat
toBase d x = reverse (toBase' d x)
  where
  	-- Het basisgeval
    toBase' d 0 = []
    -- Deel het getal door het grondtal, zet de rest in de lijst en ga door met
    -- de uitkomst.
    toBase' d y = let (a,b) = quotRem y d in [b] ++ toBase' d a


-----------------
--  Opgave  7  --
-----------------

--numbers :: Int -> [String] -> [(String,Int)]

-----------------
--  Opgave  8  --
-----------------

--grayCode :: Int -> ([Char] -> Int,Int -> [Char])

-----------------
--  Opgave 10  --
-----------------

keithGetal :: [Int] -> [Int]
-- Het basisgeval
keithGetal [] = []
-- Pak het eerste getal uit de lijst en geef dat aan de functie getal, pak daarna
-- het volgende element uit de lijst
keithGetal (x:xs) = getal(reverse(toDec x)) ++ keithGetal xs

getal :: [Int] -> [Int]
-- Deze functie berekent de som van de voorgaande getallen in de lijst.
getal n = getal (m:n)
		where m = sum(take 2 n)







