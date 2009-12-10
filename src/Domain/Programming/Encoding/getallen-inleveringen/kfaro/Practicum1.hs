{- 	Practicum 1 Functioneel Programmeren
	By: Koen Faro
	Student #: 3352048
	CS username: kfaro
	Used compiler: GHC 6.10.1
-}

{- Imports -}

module Practicum1 where

import Char
import Data.Char
import List
import Control.Arrow

{- Opdracht 1 -}

{- We krijgen een lijst van integers als argument, door hier het eerste getal te vermenigvulden met 10^(lengte rest) en dit steeds op te tellen krijgen we het eindgetal -}

fromDec :: [Int] -> Int
fromDec [] = 0
fromDec (x:xs) = result + fromDec xs
    where result = x * (10 ^ length xs)

{- Opdracht 2 -}

{- We krijgen een getal als argument, als we hier de deelrest van nemen dan weten we het laatste getal, het aantal maal dat hier 10 uit kan worden gehaald wordt gelooped en weer deelrest 
   bepaald totdat dit niet meer kan 
-}

toDec :: Int -> [Int]
toDec 0 = []
toDec n = toDec (quot n 10) ++ result
    where result = [mod n 10]

{- Opdracht 3 -}

{- Zelfde als opdracht 1, maar dan met 2 als grondtal -}

fromBin :: [Int] -> Int
fromBin [] = 0
fromBin (x:xs) = result + fromBin xs
    where result = x * (2 ^ length xs)
	
{- Opdracht 4 -}

{- Zelfde als opdracht 2, maar dan met 2 als grondtal -}

toBin :: Int -> [Int]
toBin 0 = []
toBin n = toBin (quot n 2) ++ result
    where result = [mod n 2]
	
{- Opdracht 5 -}

{- We krijgen een grondtal en een string/lijst met chars waarvan we op basis van het grondtal het decimale getal moeten bepalen, eerst moeten we kijken of de chars wel binnen het grondtal vallen
   ([f,f,f] met grondtal 10 gaat niet werken ;)), dit is toevoegd voor opdracht 7we moeten de char omzetten naar een int voordat we er iets mee kunnen, had hier ook digitToInt kunnen gebruiken 
   maar had dit nog niet door en heb zelf een functie convertToInt geschreven, validateBaseInput bekijkt of de chars binnen het grondtal vallen door ze om te zetten naar hun chr waardes. 
   Dit is trouwens wellicht niet de effectiefste manier om te werken, we valideren elke cycle de input weer..
 -}

fromBase :: Int -> [Char] -> Int
fromBase g [] = 0
fromBase g (x:xs) = if validateBaseInput g (x:xs) then result + fromBase g xs else 0
    where result = convertToInt g (toLower x) * (g ^ length xs)

convertToInt :: Int -> Char -> Int
convertToInt g x = if isDigit x then digitToInt x else if ((ord x - 86) <= g) then (ord x - 87) else -1

validateBaseInput :: Int -> [Char] -> Bool
validateBaseInput g [] = False
validateBaseInput g (xs) = if any (`notElem` validChars) xs then False else True
   where validChars = if g < 10 then map toEnum [48..g+48] else map toEnum [48..58] ++ map toEnum [97..97+(g-11)]

{- Opdracht 6 -}

{- Omgekeerde van opdracht 5 met als basis opdracht 2, geen check nodig want een getal is altijd goed als input, convertToChar geschreven om een int om te zetten naar de chr waarde. -}

toBase :: Int -> Int -> [Char]

toBase g 0 = []
toBase g n = toBase g (quot n g) ++ result
    where result = [convertToChar(mod n g)]

convertToChar :: Int -> Char
convertToChar x = if x > 9 then chr (x+87) else chr (x+48)

{- Opdracht 7 -}

{- Hiervoor opdracht 5 moeten herzien, zodat er bij een ongeldige string niets terugkomt, verder niet zo spannend... -}

numbers :: Int -> [String] -> [(String,Int)]
numbers g [] = [];
numbers g (x:xs) = result ++ numbers g xs
    where result = if stringresult > 0 then [(x, stringresult)] else []
		where stringresult = fromBase g x
		
{- Opdracht 8 -}

{- Lastig, beetje onduidelijke omschrijving, op wikipedia staan verschillende gray-systemen, er word gesproken over 'alle beschikbare cijfers gebruiken', betekend dit zogenaamde 'n-ary' Gray-code? 
   Vanwege tijds-onderschatting en onduidelijkheid dus maar overgeslagen 
-}


{- Opdracht 9 -}

{- Bij deze opdracht hulp ingeschakeld van een vriend die mij vooral het gebruik van de (&&&) operator moest uitleggen. Denk niet dat ik hier zelf helemaal uit was gekomen, snap nu de werking wel:
   &&& zorgt ervoor dat alle twee de functies waarmee hij word aangeroepen worden toegepast op het argument. Dus ((+2) &&& (*3)) 6 doet:  6+2 en 6*3 en heeft dus als resultaat (8,18).
   De functie van map zorgt ervoor dat de string base (de integer als een string) wordt gezien als een integer, als we dit niet deden dan kan group hier niets mee.
   Group is de functie waar heel het algoritme om draait, die splitst een lijst in kortere lijsten die steeds hetzelfde bevatten, door de lengte van deze lijsten weten we dan hoeveel van een bepaald karakter voorkomt, door er hier 1 van te 'taken' weten we ook welk cijfer het was.
   De output is op dit moment een lijst van Int's, ik krijg het niet voor elkaar om mijn code zo om te bouwen dat hij een lijst van Strings teruggeeft, waarschijnlijk lukt dit met een andere aanpak
   wel, de functie intsToStrings in ieder geval wel geschreven.
 -}

lookAndSay :: Int -> [Int]
lookAndSay g = map (read :: String -> Int) (iterate (concatMap (uncurry (++) . ((show . length) &&& take 1)) . group) base)
	where base = intToString g

intToString :: Int -> String
intToString 0 = ""
intToString n = intToString (quot n 10) ++ result
    where result = [intToDigit(mod n 10)]
	
intsToStrings :: [Int] -> [String]
intsToStrings [] = [];
intsToStrings (x:xs) = result ++ intsToStrings xs 
    where result = [intToString(x)]


{- Opdracht 10 -}

{- Vriend die me aan het helpen was heeft het voor elkaar gekregen, de code die hij echter had geklopt ging mij ver boven de pet, aangezien het hier om mijn vindingrijkheid en begrip gaat heb ik 
   deze opdracht dus ook maar laten varen.
-}
