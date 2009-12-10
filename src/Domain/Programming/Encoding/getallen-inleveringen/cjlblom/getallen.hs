{- Functioneel Programmeren
   Practicum 1 : Getallen
   Chris Blom 3020312
   Compiler: GHC 6.8.2   -}

module Getallen where

import Data.Char

-- Opgave 1:
-- [A,B,C] <-> A*10^2 + B*10^1 + C*10^0 <-> ((((0*10)+ A)*10)+B)*10)+C
-- uitleg: vervang de (:) met (+).(10*) met behulp van foldl
fromDec :: [Int] -> Int
fromDec = foldl ( (+).(10*) ) 0

-- Opgave 2:
-- haalt recursief het laatste cijfer van een getal en stop het achteraan de lijst
toDec :: Int -> [Int]
toDec x
 | x < 10    = [x]
 | otherwise = toDec (x `div` 10) ++ [x `mod` 10]

-- Opgave 3:
-- [A,B,C] <-> A*2^2 + B*2^1 + C*2^0 <-> ((((0*2)+ A)*2)+B)*2)+C
-- uitleg: vervang de (:) met (+).(2*) met behulp van foldl
fromBin :: [Int] -> Int
fromBin = foldl ( (+).(2*) ) 0

-- Opgave 4:
-- haalt recursief het laatste cijfer van een getal en stop het achteraan de lijst
toBin :: Int -> [Int]
toBin x
 | x < 2     = [x]
 | otherwise = (toBin (x `div` 2)) ++ [x `mod` 2]

-- Opgave 5:
-- [A,B,C] base <-> A*base^2 + B*base^1 + C*base^0 <-> ((((0*base)+ A)*base)+B)*base)+C
-- uitleg: vervang de (:) met (+).(base*) met behulp van foldl
-- en map daarbij van tevoren de chars naar ints
fromBase :: Int -> [Char] -> Int
fromBase base list = foldl ((+).(*base)) 0 (map toInt list)

-- Opgave 6:
toBase :: Int -> Int -> [Char]
toBase base x
   | x < base  = [toDigit x]
   | otherwise = (toBase base (x `div` base)) ++ [toDigit (x `mod` base)]


-- toInt : 
-- neemt als arument een char en geeft de bijbehorende int terug
-- Alleen correct voor alfanumerieke waarden, waarbij '0'..'9' naar
-- 0..9 gemapt worden en 'a'..'z' naar 10..35
toInt :: Char -> Int
toInt c = let uc = toLower c in 
   if isDigit uc then (ord uc)-48
                 else (ord uc)-87

-- toDigit :
-- neemt als argument een int en geeft de bijbehorende char terug.
toDigit :: Int -> Char
toDigit x = if x < 10 then chr (x+48) else chr (x+87)

-- Opgave 7:
numbers :: Int -> [String] -> [(String,Int)]
numbers base = map $ \x -> (x,fromBase base x)


-- opgave 8:

baseToGray :: Int -> Int -> [Int]
baseToGray base x =  baseToGray' base (map toInt (toBase base x)) 0
baseToGray' _ [] _ = []
baseToGray'  base  (bh:bt)  shift = graydigit :(baseToGray'  base bt (shift+graydigit))
  where graydigit = (bh+base-shift)`mod` base

grayToBase :: Int -> [Char] -> [Int]
grayToBase base graycode = grayToBase' base (map toInt graycode) 0

grayToBase' :: Int -> [Int] -> Int -> [Int]
grayToBase' _ [] _ = []
grayToBase'  base (gh:gt) shift = basedigit : (grayToBase' base gt (shift+basedigit))
   where basedigit = (gh-base+shift) `mod` base


grayCode :: Int -> ([Char] -> Int , Int -> [Char])
grayCode base = ( \x -> fromBase base (map toDigit (grayToBase base x)),
                  \x -> map toDigit ((baseToGray base) x) )

-- Opgave 9:
--
lookAndSay :: Int -> [String]
lookAndSay x =  map (map toDigit) (recount [x])

-- genereer de (oneindige) lijst van lookandsay getallen
recount x = [x] ++ (recount $ count x)

-- initialiseerde de accumulator met het eerste element en 0
count list =  (account 0 (head list) list)

-- genereert het volgende lookandsy getal door
-- te kijken hoe vaak welke getallen voorkomen in de lijst
account acc element [] = [acc,element]
account acc element (h:t) =
   if element == h                         -- als h en het getal dat nu geteld wordt gelijk zijn
   then (account (acc+1) h t)              -- ga door met een vehoogde teller
   else (acc:element:(account 1 h t))      -- anders: het getal dat nu geteld, met de teller ervoor 
                                           -- in de lijst, en ga door

{- Opgave 10:
 de reeks van alle Keith-getallen is:
 de reeks van alle getallen (vanaf 10) die voldoen aan het isKeith predicaat
 -}
keithGetallen :: [Int]
keithGetallen = filter isKeith [10..]

-- x is een Keithgetal als het voorkomt in zijn eigen reeks
-- en groter is dan 9
isKeith x = (inSequence x $ keithSequence x) && (x > 9)

-- x komt voor in een oplopende reeks als er een element in voorkomt dat
-- dezelfde waarde heeft. Als we een element met een hogere waarde tegenkomen,
-- komt x er zeker niet in voor omdat de reeks oplopend is.
inSequence x (h:t)
   | h == x  = True
   | h >  x  = False
   | otherwise = inSequence x t

-- genereer de keithsequence door een getal om te zetten naar decimale representatie
-- en de reeks op te bouwen met nextKeith aan de hand van de lengte van die decimale
-- representatie
keithSequence digits = start ++ nextKeith (length start) start
   where start = toDec digits

-- genereer de reeks door de volgende waarde te berekenen
-- en recursief toe te voegen achteraan de reeks
-- De volgende waarde in de reeks is de som van de n laatse waardes
nextKeith n seq =  nextvalue ++ (nextKeith n (seq ++ nextvalue) )
   where nextvalue = [sum (take n (reverse seq))]
