module Practicum1rhouweli where 

import Char
import List
--(Ik heb de ghci compiler gebruikt tijdens het maken van dit practicum)



--opgave 1


{- Hier volgt mijn eerste poging:

fromDec :: [Int] -> Int
fromDec [] = 0
fromDec [x] = x 
fromDec (x:xs) = getal + fromDec xs
        where getal = x* (10 ^ length xs) 

Wat er bebeurt is dat het eerste getal uit de lijst wordt gehaald,
vervolgens wordt deze vermenigvuldigd met 10 tot de macht (lengte van de rest van de lijst).
Zo wordt 4,3,2 dus bijvoorbeeld: 4*10^2= 400. 
3,2 blijft dan over, daar gebeurt hetzelfde mee: 3*10^1= 30.
Dan blijft er de 2 over. deze drie getalleen worden vervolgens bij elkaar opgeteld:
400+30+2= 432. Zo wordt een lijst met de getallen 4,3,2 dus omgezet in de integer 432.

later kwam ik er achter dat het veel korter kan door foldl als hogere orde functie te gebruiken: -}

fromDec :: [Int] -> Int
fromDec = foldl (\ x y -> 10* x + y) 0



--opgave 2


toDec :: Int -> [Int]
toDec 0 = []
toDec x = toDec (quot x 10)++[mod x 10]

{- Om te beginnen wordt de functie mod x 10 toegepast. Dit zorgt voor de rest bij deling bij de deling x/10.
De functie quot deelt het eerste argument door de tweede en verwerpt de restwaarde bij de deling.
Zo blijft er een reeks getallen over die door de operator (++) aan elkaar worden geplakt in een lijst,
en het gewenste resultaat is daar. -}



--opgave 3


fromBin :: [Int] -> Int
fromBin [] = 0
fromBin [x] = x 
fromBin (x:xs)= getal + fromBin xs
        where getal = x * (2 ^ length xs)

{- Bij deze opgave gebeurt bijna hetzelfde als bij mijn eerste poging voor opgave 1,
het enige verschil is dat het getal niet wordt vermenigvuldigd met een 10 macht, 
maar met een 2 macht:
[1,0,1] levert 1*2^2= 4 op,
[0,1] levert 0*2^1= 0 op,
en er blijft een 1 over. Tel deze drie getallen bij elkaar op: 
4+0+1= 5. Een lijst met de binaire cijfers 1,0,1 wordt dus omgezet naar het getal 5. -}



--opgave 4


toBin :: Int -> [Int]
toBin 0 = []
toBin x = toBin (quot x 2)++[mod x 2]

{- Net als opgave 3 en 1 met elkaar overeenkomt, komt deze opgave overeen met opgave 2.
Het is ook hier het geval dat ik in plaats van de 10 een 2 heb gebruikt. 
Voor de rest is de werking hetzelfde. -}



--opgave 5


fromBase :: Int -> [Char] -> Int
fromBase g [] = 0
fromBase g (x:xs) = result + fromBase g xs
        where result = toInt g (toLower x)*(g ^ length xs)
toInt :: Int -> Char -> Int
toInt g x = if isDigit x then digitToInt x else if ((ord x-86) <= g) then (ord x-87) else 0

{- Voor de functie fromBase is nog een tweede functie nodig, die ik toInt heb genoemd. 
Deze zorgt ervoor dat de symbolen worden omgezet naar de juiste interpretatie.
Vervolgens wordt alles bij elkaar opgeteld door de fromBase functie. -}



--opgave 6


toBase :: Int -> Int -> [Char]
toBase g 0 = []
toBase g n = toBase g (quot n g) ++ result
        where result = [intToChar (mod n g)]
intToChar :: Int -> Char
intToChar x = if x > 9 then chr (x+87) else chr (x+48)

{- Deze functie werkt als het omgekeerde van opgave 5. -}



--opgave 7


numbers :: Int -> [String] -> [(String,Int)]
numbers _ [] = []
numbers x (y:yz) = if aannemelijk x y then (y,fromBase x y): numbers x yz else numbers x yz

aannemelijk :: Int -> String -> Bool
aannemelijk x [] = True
aannemelijk x (y:yz) = ord y - 87 <= x && aannemelijk x yz

{- Voor de werking van de numbers functie is een extra functie nodig, welke ik aannemelijk heb genoemd.
Deze functie controleert of de ingevoerde woorden wel aan de eisen van het grondgetal voldoen.
De overeenkomende getallen zijn simpelweg met de eerder gemaakte fromBase functie te berekenen. -}



--opgave 8

{- Is me niet gelukt. -}



--opgave 9

{- Is me niet gelukt. -}



--opgave 10

{-Deze werkt niet maar komt wel aardig in de buurt denk ik...
keithGetallen :: [Int]
keithGetallen x = getal x 0 
        where getal x y = keith (toDec x)
        keith x y = if  y < (length x) then x !! y else nogEenFunctie x y (length x)
                where nogEenFunctie x y z = if z == 0 then 0 else (nogEenFunctie x (y-1) (z-1)) + keith x (y-1)
-}
