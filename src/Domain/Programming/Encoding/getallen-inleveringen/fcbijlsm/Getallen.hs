module Getallen where
import Char
{-
 Frank Bijlsma 3251411
 Gecompileerd met Helium
 -}

-- Opgave 1
fromDec :: [Int] -> Int
fromDec [] = 0                                 -- basisgeval
fromDec (x:xs) = x*(10^length xs) + fromDec xs -- head*10^lengte rest + de waarde van de rest van de lijst. Bijv 3de element in de lijst => *10^2, 2de => *10 etc.

-- Opgave 2
toDec :: Int -> [Int]
toDec 0 = []                                  -- basisgeval
toDec x =  toDec (x `div` 10) ++ [x `mod` 10] -- Begin achteraan, neem modulo van delen door 10 en stop dat in de lijst. Deel daarna het getal door 10 en neem dan weer de modulo van 10 etc.

-- Opgave 3
fromBin :: [Int] -> Int
fromBin [] = 0                             --basisgeval
fromBin (x:xs)
    |x == 1 = (2^(length xs)) + fromBin xs --Als de head 1, tel dan 2^lengte rest op bij de rest. Bij 0 ga je gewoon verder zonder wat op te tellen.
    |otherwise = fromBin xs

-- Opgave 4
toBin :: Int -> [Int]
toBin 0 = []                              -- basisgeval
toBin x = toBin(x `div` 2) ++ [x `mod` 2] -- Begin achteraan, neem modulo van delen door 2 en stop dat in de lijst. Deel daarna het getal door 2 en neem dan weer de modulo van 2 etc.

-- Opgave 5
-- Deze functie is naief, hij gaat ervan dat het de string en de base met elkaar in overeenstemming zijn.
fromBase :: Int -> [Char] -> Int
fromBase _ [] = 0  -- basisgeval
fromBase x (y:ys)  -- berekend waarde head en telt daar waarde rest van de rij bij op (via recursie).    (charToInt y)*(x^length ys) + fromBase x ys
     |waardeY <= x = waardeY*(x^length ys) + fromBase x ys
     |otherwise  = 0
     where waardeY = charToInt y

charToInt :: Char -> Int    -- Methode die een Char omzet naar een Int
charToInt y  
     |y == '0' = 0
     |y == '1' = 1
     |y == '2' = 2
     |y == '3' = 3
     |y == '4' = 4
     |y == '5' = 5
     |y == '6' = 6
     |y == '7' = 7
     |y == '8' = 8
     |y == '9' = 9
     |y == 'a' = 10
     |y == 'b' = 11
     |y == 'c' = 12
     |y == 'd' = 13
     |y == 'e' = 14
     |y == 'f' = 15
     |y == 'g' = 16
     |y == 'h' = 17
     |y == 'i' = 18
     |y == 'j' = 19
     |y == 'k' = 20
     |y == 'l' = 21
     |y == 'm' = 22
     |y == 'n' = 23
     |y == 'o' = 24
     |y == 'p' = 25
     |y == 'q' = 26
     |y == 'r' = 27
     |y == 's' = 28
     |y == 't' = 29
     |y == 'u' = 30
     |y == 'v' = 31
     |y == 'w' = 32
     |y == 'x' = 33
     |y == 'y' = 34
     |y == 'z' = 35

-- Opgave 6
-- Deze functie is naief, hij gaat ervan dat het de int en de base met elkaar in overeenstemming zijn.
toBase :: Int -> Int -> [Char]
toBase _ 0 = []                                            -- basisgeval
-- Begin achteraan, neem modulo van delen door de base en stop dat in de lijst. Deel daarna het getal door de base en neem dan weer de modulo van de base etc.
toBase x y = toBase x (y `div` x) ++ [intToChar(y `mod` x)]

intToChar :: Int -> Char          -- Methode die een Int omzet in een Char
intToChar y 
     |y == 0 ='0'
     |y == 1 = '1'
     |y == 2 = '2'
     |y == 3 = '3'
     |y == 4 = '4'
     |y == 5 = '5'
     |y == 6 = '6'
     |y == 7 = '7'
     |y == 8 = '8'
     |y == 9 = '9'
     |y == 10 = 'a'
     |y == 11 = 'b'
     |y == 12 = 'c'
     |y == 13 = 'd'
     |y == 14 = 'e'
     |y == 15 = 'f'
     |y == 16 = 'g'
     |y == 17 = 'h'
     |y == 18 = 'i'
     |y == 19 = 'j'
     |y == 20 = 'k'
     |y == 21 = 'l'
     |y == 22 = 'm'
     |y == 23 = 'n'
     |y == 24 = 'o'
     |y == 25 = 'p'
     |y == 26 = 'q'
     |y == 27 = 'r'
     |y == 28 = 's'
     |y == 29 = 't'
     |y == 30 = 'u'
     |y == 31 = 'v'
     |y == 32 = 'w'
     |y == 33 = 'x'
     |y == 34 = 'y'
     |y == 35 = 'z'

-- Opgave 7
numbers :: Int -> [String] -> [(String, Int)]
numbers _ [] = []                                                    --basisgeval
numbers x (y:ys) |onderBase x y = (y, fromBase x y) : numbers x ys   --Als het meegeven base type niet klopt voor dit woord wordt het overgeslagen, anders wordt het in de rij gestopt. Daarna gaat de functie verder met het volgende woord.
                 |otherwise     = numbers x ys

     -- Functie die kijkt of alle elementen in een getal kleiner dan het base type dat gegeven zijn is, zodat bijvoorbeeld bij numbers 16 ["ace"; "face"; "faces"], faces geblokkeerd wordt.
onderBase :: Int -> [Char] -> Bool
onderBase _ [] = True
onderBase x (y:ys)| x >= waardeY = True && onderBase x ys
                  |otherwise = False
                   where waardeY = charToInt y
-- Opgave 8
--grayCode :: Int -> ([Char] -> Int, Int -> [Char])


-- Opgave 9
{-
lookAndSay :: Int -> [String]
lookAndSay x = iterate voegVolgendElementToe [(showInt x)]
-} 
--Dit bovenstaande stukje code werkt net niet helemaal, aangezien hij nu ipv een element aan de oorspronkelijke lijst toe te 
--voegen een nieuwe lijst met het element erbij maakt. Ik had geen tijd meer om dit te maken, maar laat toch de andere methoden 
--maar zien, want volgens mij werken die wel. Het probleem ligt hem dus bij het itereren over de lijst.
-- neemt de lijst tot nu toe gegenereerde look-and-say getallen en plakt daar de volgende achter.
voegVolgendElementToe :: [String] -> [String]
voegVolgendElementToe x = x ++ [volgendeElementLook(last(x))] --het laatste element in de lijst wordt gebruikt om zijn opvolger te generen.

{- Deze functie berekent aan de hand van een string zijn opvolger, hij kijkt telkens hoeveel elementen van de zelfde soort voor in staan, die haalt hij eruit en dan plakt hij de lengte + het element dat hij geteld heeft samen in een lijst.
 - Vervolgens doet hij dit weer op de string die overblijft en dat plakt hij dan achter de vorige rij elementen + hun lengte. -}
volgendeElementLook :: String -> String
volgendeElementLook "" = ""
volgendeElementLook x = ([intToChar(length zelfdeElementenBegin)]++[head(zelfdeElementenBegin)]) ++ volgendeElementLook restElementen
                        where zelfdeElementenBegin = takeWhile( ==head(x)) x --Alle elementen aan het begin van een string die hetzelfde zijn.
                              restElementen = dropWhile( ==head(x)) x        --Alle elementen die over zijn.


-- Opgave 10
keithGetallen :: [Int]
keithGetallen = filter isKeithGetal [10..] -- loopt alle natuurlijke getallen vanaf 10 af en filtert alle die een Keith getal zijn.


{- Checkt of een getal een keithGetal is door telkens de laatste n op te tellen,
 - wanneer er uiteindelijk een getal uitkomt dat groter of gelijk is aan x wordt getest of dat x is,
 - als het zo is dan is de het begingetal een Keithgetal, is het niet zo dan is het dat niet. -}
isKeithGetal :: Int -> Bool
isKeithGetal x 
      |last(laatsteElement) == x = True   -- until geeft iets in lijst vorm terug, dus wordt het laatste element vergeleken met het getal dat getest wordt, zijn ze het zelfde is het een Keith getal
      |otherwise                 = False
      where laatsteElement = until (testLaatsteElement x)(verlengLijst(length xlijst)) xlijst -- de n is de lengte van de xlijst, en de beginlijst is de xlijst.
               where xlijst = toDec x                                                         -- xlijst is het het getal in een lijst zoals in de eerste opgave moest.

{- plakt het volgende getal in de lijst -}
verlengLijst :: Int -> [Int] -> [Int]
verlengLijst n y = y ++ [(volgendeElementLijst y n)]

{-berekent wat het volgende getal is door de laatste n bij elkaar op te tellen. -}

volgendeElementLijst :: [Int] -> Int -> Int
volgendeElementLijst _ 0 = 0  --Basisgeval
volgendeElementLijst (x) n = last x + volgendeElementLijst (init x) (n-1) -- Neem het laatste element en tel daar het laaste element van de init bij op 
                                                                          -- totdat je het n keer gedaan hebt, resultaat is volgende waarde
{- test of het laatste Element groter of gelijk is aan x -}
testLaatsteElement :: Int -> [Int] -> Bool
testLaatsteElement x y = last(y) >= x


















