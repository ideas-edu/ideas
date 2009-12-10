-- Practicum 1, door Jordi van Duijn (3344789) en Ingo van Duijn (3344797)
-- Gebruikte compiler: GHC

-- opgave 1
-- Deze opgave heeft geen uitleg nodig, hij spreekt voor zich.

fromDec			:: [Int] -> Int 
fromDec []		=  0
fromDec (x:xs)	=  x * 10 ^ length xs + fromDec xs


-- opgave 2
-- Net als opgave 1, is hier geen uitleg nodig.

--toDec		:: Int -> [Int]
toDec 0		= []
toDec n		= toDec (div n 10) ++ [rem n 10]


-- opgave 3
-- Opgave 1 waarbij de 2 de 10 vervangt?

fromBin			:: [Int] -> Int 
fromBin []		=  0
fromBin (x:xs)	=  x * 2 ^ length xs + fromBin xs


-- opgave 4
-- Opgave 2 waarbij de 2 de 10 vervangt?

toBin		:: Int -> [Int]
toBin 0		= []
toBin n		= toBin (div n 2) ++ [rem n 2]


-- opgave 5
-- Om de fromBase functie te kunnen laten werken, moesten we een 
-- hulpfunctie 'charToInt' definiëren, die van een gegeven character
-- de bijbehorende Integer teruggeeft (voor een 'a' zal een 10 terug
-- worden gegeven)

charToInt	:: Char -> Int
charToInt c	 
    | l1 < 11 		= l1 - 1 -- is de lengte van '0' t/m geveven c < 11?
    | l2 < 27 && l2 > 0 = l2 + 9
    | l3 < 27 && l3 > 0 = l3 + 9
    | otherwise 	= 0
    where l1 = length['0'..c]
	  l2 = length['A'..c] -- ook maar even voor hoofdletters.
	  l3 = length['a'..c]

-- op het 'charToInt' elementje na, is de fromBase functie de algemene
-- vorm van fromDec en fromBin.	  
fromBase	:: Int -> [Char] -> Int
fromBase b []	= 0
fromBase b (x:s)= charToInt x * b ^ length s + fromBase b s
		  

-- opgave 6
-- Omdat het resultaattype van toBase een String is, moet er een functie
-- intToChar gedefiniëerd worden die het resultaat van integers in een
-- resultaat van Characters om kan zetten.

intToChar	:: Int -> Char
intToChar n
    | n < 10 	= ['0'..'9'] !! n
    | otherwise = ['a'..'z'] !! (n - 10)

-- In onderstaande functie is 'tBFunc' de echte toBase functie, de removeZero
-- staat er alleen maar om na de omzetting van een getal groter dan 0, de '0'
-- weg te halen die vooraan komt te staan omdat de tBFunc van elk getalletje 0
-- dat hij krijgt een character '0' vooraan de lijst plakt. (zonder removeZero
-- zou de uitkomst van toBase 2 1, "01" zijn in plaats van  "1".
	
toBase		:: Int -> Int -> [Char]
toBase b n	= removeZero(tBFunc b n)
	where tBFunc b 0 	= "0"
	      tBFunc b n 	= tBFunc b (div n b) ++ [intToChar (rem n b)]
	      removeZero [] = []
	      removeZero (x:xs)
			| x == '0' && length (x:xs) > 1 = xs
			| otherwise = (x:xs)



-- opgave 7

-- Als de toBase van een fromBase van een getal gelijk is aan zichzelf was het een valide weergave van een getal in de bijbehorende basis, en wordt deze toegevoegd aan de lijst, anders natuurlijk niet.
numbers	:: Int -> [String] -> [(String,Int)]
numbers b []	= []
numbers b (x:xs)
    | x == toBase b (fromBase b x) = (x,fromBase b x) : numbers b xs
    | otherwise = numbers b xs
	

-- opgave 8
-- Voor de grayCode hebben we een 5tal hulpfuncties gedefiniëerd. Boven
-- elke hulpfunctie staat uitleg.

-- grayFunc is de functie die al het werk doet. Met een geven grondtal en
-- een String (van getalletjes) geeft grayFunc de andere versie van de 
-- String terug, dat is, van bv een gewoon decimaal getal, de Gray-versie
-- maar ook van een Gray-versie het gewone getal. De functie is dus de
-- inverse van zichzelf! Voorbeeld:
-- grayFunc 10 "10" = "19" en grayFunc 10 "19" = "10"
-- grayFunc maakt gebruik van 'reverseList', zie hieronder wat deze functie
-- inhoudt. Er zit ook een klein oneffenheidje in de grayFunc, zie convert
-- voor extra informatie.
grayFunc 			:: Int -> [Char] -> [Char]
grayFunc x []		= []
grayFunc x (c:cs) 	= if even (charToInt c) then c : (grayFunc x cs)
                      else c : (grayFunc x (reverseList x cs))
-- reverseList maakt van een String van getalletjes en een bijbehorend grondtal
-- de omgekeerde versie van het getal, dat is, bijvoorbeeld in het binaire stelsel:
-- elke 1 wordt een 0 en elke 0 wordt een 1. In het 5 - tallige stelsel:
-- elke 0 wordt een 4, elke 1 een 3, elke 2 blijft 2, elke 3 een 1, elke 4 een 0. 
-- Dus de reverseList 10 "123975" levert op "876024". De functie spreekt voor zich.
reverseList			 :: Int -> [Char] -> [Char]
reverseList x []     = []
reverseList x (c:cs) = intToChar ((x-1) - charToInt c) : (reverseList x cs)
					  
-- Omdat de grayFunc niet goed werkt als:
--			EN: het grondtal even is.
--			EN: er van een Gray-versie, een gewoon getal gemaakt moet worden.
--			EN: het eerste character uit de String van getalletjes oneven is.
-- Moest de functie convert gedefiniëerd worden. Een voorbeeld:
-- We willen van het decimale getal "100" de Gray versie hebben:
--			grayFunc 10 "100" = "190"	-> werkt goed, maar andersom:
--			grayFunc 10 "190" = "109"???-> zou "100" moeten zijn?? Wat blijkt:
-- Als er een getalletje uit de String van getalletjes oneven is, dan staat het
-- getalletje 2 plaatjes verder, verkeerd om: "109" moet eigenlijk "100" zijn en
-- "1090" moet eigenlijk "1000" zijn en "1799" moet "1700" zijn. Daarom de functie
-- Convert, die dit foutje hersteld. Dus -> convert 10 (grayFunc 10 "190") = "100"!
-- De functie spreekt voor zich.
convert				:: Int -> [Char] -> [Char]
convert x []	= []
convert x [c] 	= [c]
convert x [c,s] = [c,s]
convert x (c:cn:cnn:cs)
    | even (charToInt c) = c : (convert x (cn:cnn:cs))
    | otherwise = c : (convert x (cn : (reverseList x [cnn]) ++ cs))

-- de GrayFunc omzetten naar de gewenste versie met onderstaande types
-- (grayToDec en decToGray)
grayToDec		:: Int -> [Char] -> Int
grayToDec x cs 
    | even x = fromBase x (convert x (grayFunc x cs))
	| otherwise = fromBase x (grayFunc x cs)
	
decToGray     :: Int -> Int -> [Char]
decToGray x y = grayFunc x (toBase x y)

-- de laatste stap: grayCode met grondtal een tupel van 2 functies op laten
-- leveren.
grayCode   :: Int -> ([Char] -> Int, Int -> [Char])
grayCode x = (grayToDec x, decToGray x)


-- opgave 9

-- Verdeel de lijst met getallen onder in groepjes naast elkaar staande dezelfde getallen
stringToList [] 	= []
stringToList (c:cs)	= [(c: (takeWhile (c ==) cs))] ++ stringToList (dropWhile (c ==) cs)

-- Maak van een string van n karakters met waarde x de string "nx"
lookAndSayForm :: [Char] -> [Char]
lookAndSayForm cs	= toBase 10 (length cs) ++ [(head cs)]

-- Zet alle groepjes naast elkaar staande dezelfde getallen in de look and say vorm en plak deze aan elkaar
lASFonList :: [Char] -> [Char]
lASFonList cs = concat (map (lookAndSayForm) (stringToList cs))

-- Maak de lijst
createLASList :: [Char] -> [[Char]]
createLASList cs = cs:(createLASList (lASFonList cs))

-- Nu met een Int als startwaarde
lookAndSay :: Int -> [[Char]]
lookAndSay x 		= createLASList (toBase 10 x)

-- opgave 10

-- Pak alle natuurlijke getallen groter of gelijk aan 10 en kijk of deze keith getallen zijn dmv filter en isKeithGetal
keithGetallen :: [Integer]
keithGetallen = filter isKeithGetal [10..]

-- Roep een functie aan die het te bepalen getal heeft, een begin van de bijbehorende keith reeks en de lengte van het getal
isKeithGetal :: Integer -> Bool
isKeithGetal g = evalueerRij g list (length list)
    where list = toDec g

-- Deze functie vergelijkt het laatste getal uit de keith reeks met het getal en roept zichzelf aan met een langere lijst als het getal nog niet voorgekomen kan zijn
evalueerRij :: Integer -> [Integer] -> Int -> Bool
evalueerRij g li le
    | g<(last li) = False
    | g==(last li) = True
    | otherwise = evalueerRij g (maakRijLanger li le) le

-- Maak de keith reeks aan de hand van de lengte van het getal één element langer
maakRijLanger :: [Integer] -> Int -> [Integer]
maakRijLanger li le = li ++ (sum (take le (reverse li))):[]