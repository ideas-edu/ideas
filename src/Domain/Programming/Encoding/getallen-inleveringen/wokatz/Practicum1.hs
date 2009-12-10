{-
Wouter Katz, 3081192
compiler: Helium
-}

module Practicum1 where 
import Char
import List

-- simpele recursieve vermenigvuldiging, x maal 10^(lengte van tail lijst), zodat bij het laatste element in de lijst x maal 10^0 uitkomt
fromDec::[Int] -> Int
fromDec [] = 0
fromDec (x:xs) = x * y + fromDec xs
                 where y | length xs > 0 = (10 ^ (length xs))
                         | otherwise = 1

-- steeds delen door 10 tot we bij 0 zijn, vervolgens op deze lijst modulo 10, en deze lijst omdraaien
toDec::Int -> [Int]
toDec = reverse .
        map (`rem` 10) .
        takeWhile (/= 0) .
        iterate (`div` 10)
        
-- simpele vermenigvuldiging: eerste element van de lijst maal 2^(lengte tail)
fromBin::[Int] -> Int
fromBin [] = 0
fromBin (x:xs) = x*2^y + fromBin xs
                 where y = length xs

-- toBinReversed zet y `rem` 2 op kop van een lijst, en hierachter recursief y `div` 2 herhalen. Als er 1 uitkomt, zijn we klaar, en deze lijst wordt omgedraaid met reverse                 
toBin::Int -> [Int]
toBin x = reverse $ 
          toBinReversed x
          where 
          toBinReversed 1 = [1]
          toBinReversed y = y `rem` 2 : toBinReversed (y `div` 2)
          
-- eerst controleren of x niet kleiner is dan 2, of groter dan 36 (10 getallen + 26 letters)
-- als dit goed is, dan v * (x^w), en recursief herhalen en hierbij op blijven tellen
-- als y een getal is (ord y tussen de 48 en 57), dan pakken we daar de numerieke waarde van.
-- anders pakken we van deze letter de numerieke waarde in het alfabet (a = 1) en 10 hierbij opgeteld. ord (tolower y) - ord 'W' doet dit precies.
fromBase::Int -> [Char] -> Int
fromBase _ [] = 0
fromBase x (y:ys) | x < 2 = 0
                  | x > 36 = 0
                  | otherwise = v * (x^w) + fromBase x ys
                                where w = length ys
                                      v | ord y <= 57 && ord y >= 48 = ord y - ord '0'
                                        | otherwise = ord (toLower y) - ord 'W'

-- toBaseReversed zet y `rem` x op kop van een recursieve lijst, met als rest van de lijst dezelfde functie op x (y `div` x)
-- z kijkt of y `rem` x een getal tussen de 0 en de 10 is, en zo ja, dan pakt deze het cijfer hierbij. Anders is het een letter, en pakt de functie er de letter bij.     
-- de uiteindelijke lijst wordt omgedraaid om in de goede volgorde te komen                                  
toBase::Int -> Int -> [Char]
toBase a b | a < 2 = []
           | a > 36 = []
           | otherwise = reverse $
                         toBaseReversed a b
                         where
                         toBaseReversed _ 0 = []              
                         toBaseReversed x y = z : toBaseReversed x (y `div` x)
                                              where z | zz <= 10 && zz >= 0 = chr (48 + zz)
                                                      | otherwise = chr (87 + zz)
                                                    zz = y `rem` x
                                                    
-- eerst controleren of de opgegeven string geldig is icm de opgegeven integer.
-- Als dit zo is, dan fromBase aanroepen om hier de numerieke waarde voor te krijgen, en een tupel maken, en dan de volgende string behandelen.
numbers::Int -> [String] -> [(String, Int)]
numbers _ [] = []
numbers x (y:ys) = if validString x y then (y, fromBase x y) : numbers x ys else numbers x ys

-- Controleren voor elke letter van de string of de waarde van x niet kleiner is dan de waarde van de letter.
validString::Int -> String -> Bool
validString _ [] = True
validString x (y:ys) = (x >= fromBase x (y:[])) && validString x ys

-- lijst maken van de string representatie van x op kop van de look-and-say lijst van x, dit recursief
lookAndSay::Int -> [String]
lookAndSay x = show x : lookAndSay (readInt (createLookList (show x)))

-- maakt een look-and-say string van een stringrepresentatie van een getal
-- pakt de head van de lijst, telt hoe vaak deze hierna nog voorkomt, en hetzelfde met het volgende getal na de reeks vorige getallen.
createLookList::String -> String
createLookList [] = []
createLookList (y:ys) = show z ++ y : createLookList (drop (z - 1) ys)
                        where z = countDigits y (y:ys)

-- telt het aantal zelfde tekens
countDigits::Char -> String -> Int
countDigits _ [] = 0
countDigits x (y:ys) | ord x == ord y = 1 + countDigits x ys
                     | otherwise = 0