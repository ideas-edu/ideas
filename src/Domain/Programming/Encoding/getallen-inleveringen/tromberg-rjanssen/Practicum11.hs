-- Practicum 1: Getallen
-- Timme Romberg 0477907
-- Rick Janssen 0455172
-- Gebruikte compiler: Helium 1.6



module Practicum11 where
import Char                                                     -- wordt gebruikt om char naar int om te zetten en andersom


--Getallensystemen:
--Opgave 1:
fromDec :: [Int] -> Int
fromDec ys = fD ys (length ys - 1)                              -- hierdoor wordt maar 1 keer de lengte berekent
           where fD [] _ = 0
                 fD (x:xs) n = (x * 10^n) + fD xs (n-1)

altFromDec :: [Int] -> Int
altFromDec ys = readInt (aFD ys)
              where aFD [] = []
                    aFD (x:xs) = chr (x + 48) : aFD xs

--Opgave 2:
--De Int wordt eerst omgezet in een String vervolgens wordt
-- elke char in de string omgezet in een int
toDec :: Int -> [Int]
toDec ys = tD (show ys)
         where tD [] = []
               tD (x:xs) = ((ord x) - 48) : tD xs

--Opgave 3:
-- vrijwel identiek aan fromDec
fromBin :: [Int] -> Int
fromBin ys = fB ys (length ys - 1)
           where fB [] _ = 0
                 fB (x:xs) n = (x * 2^n) + fB xs (n-1)

--Opgave 4:
toBin :: Int -> [Int]
toBin ys | ys == 0 = [0]                      -- anders werkt toBin niet met 0 (verkeerd output namelijk "")
         | otherwise = reverse (tB ys)
                     where tB 0 = []
                           tB xs = (xs - ((xs `div` 2)*2)) : tB (xs `div` 2) -- `div` wordt gebruikt omdat die functie altijd naar beneden afrond

--Opgave 5:
-- vrijwel identiek aan fromBin (met als extra parameter de base)
fromBase :: Int -> [Char] -> Int
fromBase _ [] = 0
fromBase base ys = fB base ys (length ys - 1)
                 where fB _ [] _ = 0
                       fB bs (x:xs) l = (f (ord x))*bs^l + fB bs xs (l-1)
                                        where f z | z < 58 = z -  48
                                                  | otherwise = z - 87

--Opgave 6:
toBase :: Int -> Int -> [Char]
toBase _ 0 = ['0']
toBase base xs = toBase2 base xs

toBase2 :: Int -> Int -> [Char]
toBase2 base xs = reverse (tB base xs)   -- reverse gebruikt zodat ++ niet gebruikt wordt
                where tB _ 0 = []
                      tB bs ys = ( chr (f (ys - ((ys `div` bs)*bs)))) : (tB bs (ys `div` bs))   --bs is het base getal
                              where f z | z < 10 = z + 48
                                        | otherwise = z + 87

--Opgave 7:
--maakt op een simpele en recursieve manier gebruik van fromBase
numbers :: Int -> [String] -> [(String,Int)]
numbers _ [] = []
numbers base (x:xs) = (x, fromBase base (x)) : numbers base xs

--Gray-coderingen:
--Deze graycode wijkt enigsinds af van degene beschreven in de opdracht, 0 wordt gezien als een vol geldig symbool en komt dus voor aan het begin van een gray code
--Opgave 8:
grayCode :: Int -> ([Char] -> Int, Int -> [Char])
grayCode base = (g2i, i2g)
              where g2i xs = graypos xs (graylist base)                                  -- zoekt de gemaakt graylist af en geeft de positie (en dus het getal) terug
                             where graypos _ [] = 0
                                   graypos s (z:zs) | (s == z) = 0
                                                    | otherwise = 1 + (graypos s zs)
                    i2g ys = graylist base !! (ys)                                       -- maakt een graylist aan en past !! daarop toe

-- graylist maakt een oneindige lijst van gray coderingen aan door opeenvolgende de graycoderingen van n-bit (vanaf 1) te concateneren
graylist :: Int -> [String]
graylist base = gl 0 0 base 1
              where gl c s ba bi = (grayn c s ba bi) ++  (gl 0 0 ba (bi+1))

-- grayn geeft een graycodering van n-bits (lengte)
-- counter wordt gebruikt om aan te geven welk symbool wordt toegevoegt
-- switch wordt gebruikt om aan te geven of de gegeven sub-reeks moet worden om gedraait of niet (0 niet 1 wel) (gaat om en om, voor het toevoegen van een nieuw symbool)
-- base spreekt voor zich
-- bits is de lengte van de reeks
-- counter -> switch -> base -> bits -> [String]
grayn :: Int -> Int -> Int -> Int -> [String]
grayn _ _ _ 0 = [""]
grayn counter switch base bits = (map ((head (toBase base counter)):) ((mabrev switch) prevgr)) ++ endswtch
                               where prevgr     = grayn 0 0 base (bits-1)
                                     mabrev s   | s == 0 = reverse                   -- gebruikt switch om te bepalen of reverse moet worden gebruikt of niet
                                                | otherwise = id
                                     chswitch x | x == 0 = 1                         -- draait switch om
                                                | otherwise = 0
                                     endswtch   | counter < (base - 1) = grayn (counter + 1) (chswitch switch) base bits -- kijkt of alle mogelijke symbolen zijn toegevoegt
                                                | otherwise = []

-- Look-and-say-reeksen:
-- Opgave 9:
lookAndSay :: Int -> [String]
lookAndSay x = iterate (oneStepSay) (show x)

-- berekend het eerst volgende look-and-say getal
oneStepSay :: String -> String
oneStepSay [] = []
oneStepSay h@(x:_) = (show (length (takeWhile (==x) h)) ++ [x]) ++ (oneStepSay (dropWhile (==x) h))

-- Keith-getallen:
-- Opgave 10:
-- mogelijke keithgetallen worden berekent vanaf 14 (sinds dit het eerste keithgetal is)
keithGetallen :: [Int]
keithGetallen = keithGetallen2 14                   -- mogelijke keithgetallen worden berekent vanaf 14 (sinds dit het eerste keithgetal is)
              where keithGetallen2 x | (keithList x (toDec x) (length (toDec x)) == True) = x : (keithGetallen2 (x+1))
                                     | otherwise = [] ++ keithGetallen2 (x+1)

-- maakt een keithreeks aan, zodra het laatste getal groter (false) of gelijk is (true) dan zijn eerste stopt het algoritme
keithList :: Int -> [Int] -> Int -> Bool
keithList x xs l | x > (last xs) =  keithList x (keithListStep xs l) l
                 | x == (last xs) = True
                 | otherwise = False
                 where keithListStep [y] _ = [y,y]  -- keithListStep telt de laatste l (lengte van het eerste gegeven getal) getallen op
                       keithListStep ys le = ys ++ [sum (drop (length ys - le) ys)]



