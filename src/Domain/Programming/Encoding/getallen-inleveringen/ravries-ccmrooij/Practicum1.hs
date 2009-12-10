--------------------------------------------------------------------------------
-- Name: Christian van Rooij       Student num.: 3412326       login: ccmrooij
-- Name: Robert de Vries           Student num.: 3258254       login: ravries
--------------------------------------------------------------------------------
module Practicum1 where

-- import nodig voor de functies chr en ord.
import Char

--------------------------------------------------------------------------------------------------------
                                    -- Opgave 1 fromDec --
--------------------------------------------------------------------------------------------------------

fromDec :: [Int] -> Int
fromDec [] = 0
fromDec (x:xs) = x * (10 ^ (length (x:xs) - 1)) + fromDec xs 

-- Oftewel voor alle elementen x: vermenigvuldigen met de bijbehorende 10 macht en bij elkaar optellen

--------------------------------------------------------------------------------------------------------
                                    -- Opgave 2 toDec --
--------------------------------------------------------------------------------------------------------

toDec :: Int -> [Int]
toDec_ :: Int -> [Int]

toDec getal = reverse (toDec_ getal)
toDec_ getal    | getal < 10 = getal : []
                | otherwise =  nieuw : toDec_ (getal `div` 10)
                               where nieuw = getal `mod` 10

-- Voeg ieder getal (modulo 10) toe aan de lijst van dit getal (gedeelt door 10) totdat het getal minder dan 10 is
-- Doordat op deze manier alle elementen achterstevoren worden ingevoegd, moet de totale lijst op het einde nog 
-- worden gereversed

--------------------------------------------------------------------------------------------------------
                                    -- Opgave 3 fromBin --
--------------------------------------------------------------------------------------------------------

fromBin :: [Int] -> Int
fromBin [] = 0
fromBin (x:xs)  | x <= 1 = x * (2 ^ (length (x:xs) - 1)) + fromBin xs 
                | otherwise = error "lijst mag alleen uit 1 en 0 bestaan!"

-- Voor alle elementen x; vermengivuldigen met de bijbehorende tweemacht 
-- Lengte x:xs minus 1 doordat je anders telkens een macht te hoog zit

--------------------------------------------------------------------------------------------------------
                                    -- Opgave 4 toBin --
--------------------------------------------------------------------------------------------------------

toBin :: Int -> [Int]
toBin_ :: Int -> [Int]

toBin getal = reverse (toBin_ getal)
toBin_ getal    | getal < 2 = getal : []
                | otherwise =  nieuw : toBin_ (getal `div` 2)
                                where nieuw = getal `mod` 2			

-- Idem als Opgave 2, maar dan met grondtal 2 ipv 10

--------------------------------------------------------------------------------------------------------
                                    -- Opgave 5 fromBase --
--------------------------------------------------------------------------------------------------------

fromBase :: Int -> [Char] -> Int
fromBase_ :: Int -> [Char] -> Int -> Int

fromBase stelsel lijst =  (fromBase_ stelsel  (reverse(lijst)) 0)
fromBase_ stelsel (x:xs) pos  | (ord(x) <= 57) =  ((ord(x) - 48) * stelsel ^ pos) + fromBase_ stelsel xs (pos+1)-- het is dus 0-9
                              | otherwise =       ((ord(toUpper x) - 55) * stelsel ^ pos) + fromBase_ stelsel xs (pos+1)  -- het is dus a-A tot z-Z
fromBase_ _ [] _ = 0

-- In principe hetzelfde als opdracht 1 en 3, maar dan met een variabele grondtal
-- Iedere letter in de string moet echter nog worden omgezet naar een getal, 
-- dit gebeurt mbv de functie ord, die het teken (in ASCII) naar een cijfer veranderd en corrigeert 

--------------------------------------------------------------------------------------------------------
                                    -- Opgave 6 toBase --
--------------------------------------------------------------------------------------------------------

toBase :: Int -> Int -> [Char]
toBase_ :: Int -> Int -> [Char]
toChar :: Int -> Char

toBase grondtal getal = reverse (toBase_ grondtal getal)
toBase_ grondtal getal  | getal < grondtal = toChar(getal) : []
                        | otherwise =  nieuw : toBase_ grondtal (getal `div` grondtal)
                               where nieuw = toChar(getal `mod` grondtal)

toChar x | (x >= 0 && x <= 9) = chr (x + 48)
         | (x >= 10 && x <=35) = chr(x + 87)
		 | otherwise = error "Klopt niet!" 

-- Dezelfde methode als bij opdracht 2 en 4, nu met een variabel grondtal
-- Deze keer worden de cijfers omgezet naar letters(geen hoofdletters) dmv de methode chr. 
-- cijfers -> ASCII dmv van + 48 en letters -> ASCII dmv + 87

--------------------------------------------------------------------------------------------------------
                                    -- Opgave 7 numbers --
--------------------------------------------------------------------------------------------------------

numbers :: Int -> [String] -> [(String, Int)]
subnum :: Int -> [Char] -> Bool

numbers _ []          = []
numbers grond (x:xs)  =  if (subnum grond x) then ((x, (fromBase grond x)) : numbers grond (xs) ) else numbers grond (xs)

subnum grond []     = True
subnum grond (x:xs) | (ord(toUpper x) >= 65) && (ord(toUpper x) <= (65 + grond - 10)) = subnum grond xs
                    | otherwise = False

-- Valideer eerst iedere string uit de lijst met Subnum
-- Subnum kijkt of iedere char uit de string in het gegeven grondtal geschreven kan worden,
-- M.b.v. de functie ord (kijkt of de char tussen de ASCII tekens voor hoofdletters zit - gecorrigeerd met de eerste tien normale cijfers)
-- Klopt de string, maak dan een tupple aan met die string en de omgezette cijferreeks, en ga door met de rest van de reeks

--------------------------------------------------------------------------------------------------------
                                    -- Opgave 9 lookAndSay --
--------------------------------------------------------------------------------------------------------

-- lookAndSay zorgt ervoor dat de meegegeven integer eerst omgezet wordt in een lijst van losse integers met behulp van toDec.
-- Vervolgens laten we de functie map los op elke integer in die lijst, zodat
-- de bijbehorende characters worden verkregen. Op deze manier wordt de hele integer omgezet in zijn string-formaat.

-- iterate losgelaten op bereken zorgt er vervolgens voor dat een 'oneindige' lijst wordt gecreeerd.
-- hierbij zal bereken telkens met behulp van de meegegeven argumenten het nieuwe getal berekenen.

lookAndSay :: Int -> [String]
bereken :: [Char] -> [Char]
bereken_ :: String -> Char -> Int -> String

lookAndSay int_start = (map toChar (toDec int_start)) : iterate bereken (bereken (map toChar (toDec int_start)))   
				   where toChar int_getal = chr(int_getal + 48)
				   
bereken list = bereken_ list (head(list)) 0
bereken_ [] huidig_teken aantal                            =     (chr (aantal + 48)) : huidig_teken : []
bereken_ (x:xs) huidig_teken aantal | huidig_teken == x    =      bereken_ xs huidig_teken (aantal + 1) 
                                    | otherwise    =     (chr (aantal + 48)) : huidig_teken : (bereken_ xs x 1)

--------------------------------------------------------------------------------------------------------
                                    -- Opgave 10 keithGetallen --
--------------------------------------------------------------------------------------------------------

-- keithGetallen zorgt ervoor dat 10 direct als startwaarde wordt gebruikt.
-- keithGetallen_ pakt de heads van de lijsten die door beginReeks zijn gemaakt als het getal zelf in de lijst voorkomt.
-- beginReeks introduceert telkens een nieuw getal waarvan via maakReeks een lijstje keithgetallen gemaakt wordt.
-- maakReeks produceert/berekent een lijst van getallen en doet net alsof het keithgetallen zijn, ookal kan blijken dat het niet zo is.
-- berekenUitvoorgangers krijgt de hele lijst mee en filtert alles op de elementen na waaruit het nieuwe getal moet worden berekent

keithGetallen :: [Int]
berekenUitvoorgangers :: [Int] -> Int -> [Int]
maakReeks :: Int -> [Int] -> Int -> [Int]
beginReeks :: Int -> Int -> [Int] 


keithGetallen = (keithGetallen_ 10) 

keithGetallen_ i = if(last(takeWhile (<=i) (beginReeks i ( length(toDec(i) )   ))) == i ) then i : (keithGetallen_ (i + 1)) else (keithGetallen_ (i + 1))

beginReeks getal lengte = (take (length(toDec(getal)) - 1) (toDec(getal))) ++ (maakReeks (last(toDec(getal))) (toDec(getal)) lengte)

maakReeks getal hele_lijst lengte = getal : (maakReeks opvolger (hele_lijst ++ [opvolger]) lengte)
  where   opvolger = (foldl (+) 0 (berekenUitvoorgangers hele_lijst lengte))
  
berekenUitvoorgangers int_lijst beginlengte = drop ((length int_lijst) - beginlengte) int_lijst