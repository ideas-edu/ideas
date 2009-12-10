{-Practicum 1, "Getallen"
Door: Simeon J. A. Visscher (3398544)
Gebruikte compiler: GHC(i) (6.10.1-i386-windows) -}

import Char
import Data.List
import Data.Fixed


{- Opdracht 1
Functie die voor een reeks cijfers het getal geeft dat je krijgt wanneer je
de cijfers (op volgorde) achter elkaar plaatst.
Bv. fromDec [1,2,3] = 123

NB Er wordt geen onderscheid gemaakt tussen cijfers en getallen groter dan
negen in de ingevoerde reeks. Bv. fromDec [10,10] = 110 -}
fromDec :: [Int] -> Int
fromDec[] = 0
fromDec(x:xs) = x * 10 ^ length xs + fromDec xs


{- Opdracht 2
Functie die een reeks geeft met daarin de cijfers van het getal x, op volgorde.
Bv. toDec 123 = [1,2,3] -}
toDec :: Int -> [Int]
toDec 0 = []
toDec x = toDec(div x 10) ++ [rem x 10]


{- Opdracht 3 
Functie die voor een reeks binaire cijfers het decimale getal geeft dat
equivalent is aan het binaire getal dat je krijgt wanneer je de binaire 
cijfers achter elkaar plaatst.
Bv. fromBin [1,1,0,1] = 13 (want het binaire getal 1101 is gelijk aan het
decimale getal 13)

NB Er wordt geen onderscheid gemaakt tussen binaire cijfers en getallen
groter dan 1 in de ingevoerde reeks. Bv. fromBin [5,5] = 15 -}
fromBin :: [Int] -> Int
fromBin [] = 0
fromBin (x:xs) = x * 2 ^ length xs  + fromBin xs


{- Opdracht 4
Functie die een reeks geeft met daarin de cijfers van het binaire getal dat
equivalent is aan het decimale getal x. 
Bv. toBin 13 = [1,1,0,1] (want het binaire getal 1101 is gelijk aan het
decimale getal 13) -}
toBin :: Int -> [Int]
toBin 0 = []
toBin x = toBin(div x 2) ++ [rem x 2]


{- Opdracht 5  
Functie die het decimale getal geeft, dat equivalent is aan de ingevoerde string, 
in het getallensysteem met grondtal b 
Bv. fromBase 2 "100110" geeft 38 -}
fromBase :: Int -> [Char] -> Int
fromBase b [] = 0
fromBase b (x:xs) | ord x > 90  = (ord x - 87) * b ^ length xs + fromBase b xs
                  | ord x > 57  = (ord x - 55) * b ^ length xs + fromBase b xs
				  | otherwise	= (ord x - 48) * b ^ length xs + fromBase b xs

				  
{- Opdracht 6 
Functie die het decimale getal x weergeeft als getal in het getallensysteem met grondtal b
Bv. toBase 2 38 = "100110" -}
toBase :: Int -> Int -> [Char]
toBase b 0 = []
toBase b x | rem x b > 9 = toBase b (div x b) ++ [chr (rem x b + 87)]
           | otherwise   = toBase b (div x b) ++ [chr (rem x b + 48)] 
		   

{-Opdracht 7 
Functie die de woorden uit een woordenlijst weergeeft met daarnaast de getalwaarde in het
getallensysteem met grondtal b. Indien een woord niet zo'n getalwaarde heeft wordt het 
weggelaten.
Bv. numbers 30 ["Simeon","wil","goed","cijfer"] = 
[("Simeon",695587343),("goed",454033),("cijfer",306706947)] -}
numbers :: Int -> [String] -> [(String,Int)]
numbers b [] = []
numbers b (x:xs) | filter (buitenrange b) x == [] = [(x,fromBase b x)] ++ numbers b xs 
                 | otherwise					  = numbers b xs
-- Hulpfunctie om te zorgen dat een letter daadwerkelijk in het getallensysteem moet voorkomen				 
buitenrange :: Int -> Char -> Bool
buitenrange b x = ord (toUpper x) > 54+b  
			 

{- Opdracht 8 
Geef een grondtal b, en deze functie construeert een Gray-codering, dat is, een functie om een
Graygetal te converteren naar een Int, en een functie om een Int te converteren naar een
Graygetal. -}					 
grayCode :: Int -> ([Char] -> Int, Int -> [Char])
grayCode b = (grayToInt b, intToGray b) 	  
	 
-- De functie die Int i naar een Graygetal met grondtal b converteert	 
intToGray :: Int -> Int -> [Char]				 
intToGray b i = baseToGray b (toBase b i)	
{- Twee hulpfuncties die een getal uit het getallensysteem met grondtal b naar een 
Graygetal converteren. De parameter i hier het getal met grondtal b. De parameter d is
toegevoegd om ervoor te zorgen dat nullen niet wegvallen -}
baseToGray :: Int -> [Char] -> [Char]			 
baseToGray b [] = []
baseToGray b i | c == 0 = [(head i)] ++ baseToGray b (tail i)
               | c == 1 = [(head i)] ++ baseToGray2 b (tail i) (length (tail i) - 
			          length (toBase b (b^(length ((tail i))) - 1 - fromBase b (tail i))))
            where c = mod (fromBase b [head i]) 2	
			
baseToGray2 :: Int -> [Char] -> Int -> [Char]			
baseToGray2 b [] 0 = []
baseToGray2 b i d| d > 0 = "0" ++ baseToGray2 b i (d-1)
                 | True   = baseToGray b c
            where c = toBase b (b^(length (i)) - 1 - fromBase b i)

-- De functie die een Graygetal met grondtal b converteert naar een Int			
grayToInt :: Int -> [Char] -> Int			
grayToInt b i = fromBase b (grayToBase b i 0 0)

{- Een hulpfunctie die een graygetal i met grondtal b converteert naar een normaal getal met
grondtal b. Variabele c is toegevoegd om het afwisselen tussen (b - 1 - graycijfer) 
en graycijfer te regelen (wanneer het brongetal oneven is wisselt dit om). Net als in 
baseToGray2 is d er om problemen met de nullen te voorkomen -}
grayToBase :: Int -> [Char] -> Int -> Int -> [Char]
grayToBase b [] c d = []			
grayToBase b i c d | d > 0 = "0" ++ grayToBase b i c (d-1)
                   | c == 0 = [head i] ++ grayToBase b (tail i) e (length (tail i) - 
			          length (toBase b (b^(length ((tail i))) - 1 - fromBase b (tail i))))
                   | c == 1 = toBase b (b - 1 - fromBase b [head i]) ++ grayToBase b (tail i) e (length (tail i) - 
			          length (toBase b (b^(length ((tail i))) - 1 - fromBase b (tail i))))
		    where e = mod (c+fromBase b [head i]) 2 
			
{- Opdracht 9 
Deze functie maakt de (afgezien van variabele-restricties) oneindige look-and-say reeks bij
getal i.
lookAndSay2 is eigenlijk hetzelfde, maar hier zit het eerste getal niet bij. -}
lookAndSay :: Int -> [String]
lookAndSay i = [intArrToStr [i]] ++ lookAndSay2 i
lookAndSay2 :: Int -> [String]
lookAndSay2 i = [intArrToStr (nextLnS i)] ++ lookAndSay2 (fromDec (nextLnS i))

{- Deze hulpfunctie heeft als invoer Int i, en als uitvoer de [Int] met daarin de cijfers van
het volgende look-and-say getal -}
nextLnS :: Int -> [Int]
nextLnS i = volgendeLnS 0 0 (toDec i) []

-- Doet hetzelfde als volgendeLnS, maar heeft een aantal extra variabelen voor de recursie.
volgendeLnS :: Int -> Int -> [Int] -> [Int] -> [Int]
volgendeLnS n i [] t = t ++ [n,i]
volgendeLnS 0 i s t = volgendeLnS 1 (head s) (tail s) t
volgendeLnS n i s t | i == head s = volgendeLnS (n+1) (head s) (tail s) t
                    | otherwise = volgendeLnS 1 (head s) (tail s) (t ++ [n,i])

-- De [Int] die we van nextLnS overhouden, wordt m.b.v. deze functie omgezet naar een string					
intArrToStr :: [Int] -> [Char]					
intArrToStr [] = []	
intArrToStr (x:xs) = [chr (48+x)] ++ intArrToStr xs


{- Opdracht 10 
Dit is de "oneindige" lijst met Keith-gtallen -}
keithGetallen :: [Int]
keithGetallen = filter keithGetal [1..]
-- Een functie met als uitvoer of een getal een Keith-getal is.
keithGetal :: Int -> Bool
keithGetal x = elem x (take x (keithReeks (toDec x))) && x > 9           
{- Deze functie produceert de reeks met getallen die gebruikt wordt om te bepalen 
 of een getal een Keith-getal is -}
keithReeks :: [Int] -> [Int]
keithReeks (x:xs) = [x] ++ keithReeks (xs ++ [sum (x:xs)])