module Module1 where

-- Door: Emiel Bon en Tsun-Kiet Man, kan worden geladen in Helium 1.6 (getest)

-- Deze regel is ervoor zodat Helium 1.6 het Char bestand laadt
import Char hiding ()

-- Opdracht 1
-- Werking: De functie fromDec maakt van een lijst Int's een decimaal getal. De lengte 
--          van lijst zonder het 1e element bepaald de 10-macht. Zo is [1,2,3] 
--          (10^2 * 1) + (10^1 * 2) + (10^0 * 3) = 123.  

fromDec :: [Int] -> Int
fromDec [] = 0
fromDec (x : xs) = x * 10 ^ (length xs) + fromDec xs

-- Opdracht 2
-- Werking: De functie toDec maakt van een Int x een lijst met als elementen de 
--          verschillende getallen in x, door steeds de rest bij deling als lijst
--          vast te plakken aan toDec x, met x zonder het laatste getal. 

toDec :: Int -> [Int]
toDec 0 = []
toDec x = toDec (x `div` 10) ++ [x `mod` 10]

-- Opdracht 3
-- Werking: De functie fromBin doet hetzelfde als fromDec, maar dan met het getal 2 in
--          plaats van 10

fromBin :: [Int] -> Int
fromBin [] = 0
fromBin (x : xs) = x * 2 ^ (length xs) + fromBin xs

-- Opdracht 4
-- Werking: De functie toBin doet hetzelfde als toDec, maar dan met het getal 2 in
--          plaats van 10

toBin :: Int -> [Int]
toBin 0 = []
toBin x = toBin (x `div` 2) ++ [x `mod` 2]

-- Opdracht 5
-- Werking: hetzelfde als met toDec en toBin, alleen controleert deze functie ook of alle waardes correct zijn.
--          hiermee bedoel ik dat bijvoorbeeld in een twaalftallig stelsel geen "f" kan zitten.
--          
--          Indien er foutieve waardes worden meegegeven zoals dat er een "f" wordt gegeven in een twaalftallig stelsel
--          , dan zal het resultaat een 0 worden.

fromBase :: Int -> [Char] -> Int
fromBase 0 _ = 0
fromBase 1 _ = 0
fromBase _ [] = 0
fromBase n (x : xs) | c >= 48 && c <= 57 && n <= 36                     = (c - 48) * n ^ (length xs) + fromBase n xs
                    | c >= 97 && c <= m  && n <= 36 && notInString m xs = (c - 87) * n ^ (length xs) + fromBase n xs
                    | otherwise = 0
                    where c = ord x
                          m = 86 + n

-- Onderdeel van opdracht 5
-- Werking: hiermee wordt er gecontroleert of een karakter (of een karakter met een waarde die hoger is dan de
--          desbetreffende karakter) niet in een string voorkomt.

notInString :: Int -> [Char] -> Bool
notInString _ [] = True
notInString a (x : xs) | ord x > a = False
                       | otherwise = True && (notInString a xs)

-- Opdracht 6
-- Werking: De functie toBase maakt gegeven een grondtal b en een getal x, een cijferreeks
--          met grondtal b horend bij het getal x. Eerst wordt gekeken of het grondtal
--          tussen de 0 en de 9 ligt

toBase :: Int -> Int -> [Char]
toBase _ 0 = "0"
toBase 0 _ = []
toBase 1 _ = []
toBase b x | m >= 0  && m <= 9  && b <= 36 = crop (toBase b (x `div` b) ++ [chr (m + 48)])
           | m >= 10 && m <= 36 && b <= 36 = crop (toBase b (x `div` b) ++ [chr (m + 87)])
           | otherwise = []
           where m = x `mod` b

-- Opdracht 7
-- Werking: Deze functie combineert een lijst en het antwoord samen tot een lijst van tuples. Hierbij wordt
--          er gebruikt gemaakt van de functie zip. Om een lijst met omzettingen naar een decimaal stelsel te
--          genereren is een subfunctie translate ontworpen.
--
--          Gegeven een integer en een lijst van strings, zet deze funnctie het in de functie translate en combineert
--          vervolgens de resultaten.

numbers :: Int -> [String] -> [(String,Int)]
numbers 0 _ = []
numbers 1 _ = []
numbers _ [""] = []
numbers n a = zip a (translate n a)

-- Onderdeel van opdracht 7
-- Werking: Deze functie zet een lijst van getallen in een stelsel om naar een lijst van getallen in een tientallig
--          stelsel. Door middel van gebruik te maken van voorgaande functies.
--
--          Gegeven een integer en een lijst van strings, wordt eerst de elementen van de lijst omgezet door middel
--          van de fromBase functie en daarna wordt dit process herhaalt voor de volgende elementen. Wanneer er een 0
--          wordt gevonden dan betekend het meestal dat er een symbool bestaat in het element waardoor fromBase niet
--          goed uitgevoerd kan worden. Deze worden weggefilterd.

translate :: Int -> [String] -> [Int]
translate _ [] = []
translate 1 _ = []
translate 0 _ = []
translate a (x : xs) = filter (/=0) ([fromBase a x] ++ (translate a xs))

-- Onderdeel van opdracht 8
-- Werking: Gegeven een grondtal b en een cijferreeks x berekend deze functie de opvolger 
--          van de cijferreeks in een b-ary stelsel. Met fromBase wordt de cijferreeks naar 
--          een Int omgezet, dan wordt er 1 bij opgeteld, dan wordt het getal met toBase 
--          weer naar een cijferreeks omgezet

successor :: Int -> String -> String
successor b x = toBase b (fromBase b x + 1)

-- Onderdeel van opdracht 8
-- Werking: Gegeven een initiële lijst x (lijst van de vorm ["0","1".."(grondtal-1)"), een 
--          grondtal b en een lengte n maakt de functie addReverses een lijst cijferreeksen 
--          waarvan de de opvolger steeds op 1 positie verschilt van de voorgaande, doordat 
--          eerst van 0 tot het grondtal wordt geteld, dan het getal op de positie voor dat 
--          getal wordt veranderd (op dezelfde manier), en dan weer wordt afgeteld naar 0. 
--          Dit gaat (recursief) door tot de lengte van de lijst groter is dan lengte n. De 
--          functie expand breidt de huidige lijst x steeds uit met meer getallen uit het 
--          een lijst die loopt van 0 tot aan het grondtal, stapgrootte 2, waarvan de 
--          elementen eerst naar een cijferreeks met grondtal b omgezet worden door de 
--          functie toB. Vervolgens past hij de functie f toe op elk element uit die lijst.
--          De functie f s (waar s steeds een element uit de lijst is) plakt s vast aan elk 
--          element uit de lijst x en plakt de successor van s vast aan de omgekeerde lijst 
--          van x. Die twee lijsten worden weer samengevoegd tot een nieuwe lijst x, doordat 
--          concatMap de lijst ook in z'n geheel aan elkaar plakt. Omdat de functie f s 
--          steeds voor groepjes string-getallen werkt, dus "0" en "1", "1" en "2", 
--          "9 en a" etc. zou dit steeds een lijst van een even grondtal opleveren. Daarom 
--          moet onderscheid worden gemaakt tussen een even en een oneven grondtal. Het 
--          verschil tussen de twee is dat de functie bij het oneven grondtal een duo 
--          getallen eerder stopt met de functie f, en er dan het laatste getal los aan 
--          toevoegd.  

addReverses :: [String] -> Int -> Int -> [String]
addReverses x b n | length x > n = x
                  | even b    = addReverses  expand b n
                  | otherwise = addReverses (expand ++ lastD) b n
                  where expand = concatMap f (map (toB) [0,2..b-1])
                        lastD  = map (toB (b - 1) ++) x
                        toB    = toBase b
                        f s    = map (s ++) x ++ reverse ( map ( successor b s ++) x)

-- Onderdeel van opdracht 8
-- Werking: Gegeven een grondtal b en een lengte n, maakt de funtie generateGray een lijst 
--          grayCodes, gebruikmakend van de functie addReverses. De initiële lijst die wordt 
--          meegegeven aan addReverses is een lijst van 0 tot het grondtal b, omgezet naar
--          cijferreeksen met dat grondtal. Voorwaarde is dat het grondtal groter moet zijn
--          dan 1. De functie crop wordt toegepast op alle elementen uit de lijst om er 
--          zeker van te zijn dat alle cijferreeksen hetzelfde zijn (zie volgende functie).

generateGray :: Int -> Int -> [String]
generateGray 0 _ = [""]
generateGray 1 _ = [""]
generateGray b n | b > 1     = map crop (addReverses (map (toBase b) [0..b-1]) b n)
                 | otherwise = []

-- Onderdeel van opdracht 8
-- Werking: De funtie crop verwijderd alle insignificante '0'-en die voor een cijferreeks 
--          staan. Zo maakt hij van "001010" gewoon "1010".

crop :: [Char] -> [Char]
crop [] = "0"
crop (x : xs) | x == '0' = crop xs
              | otherwise = x : xs

-- De functie codeAt kijkt, gegeven een initiële positie n, een lijst cijferreeksen
-- en een cijferreeks, op welke positie in de lijst de cijferreeks c voorkomt. Word
-- de cijferreeks niet gevonden wordt de positie met 1 opgehoogd en gaat de functie
-- verder (totdat de lijst afgewerkt is). 

codeAt :: Int -> [String] -> String -> Int
codeAt _ [] _ = 0
codeAt n (x : xs) c | x == crop c = n
                    | otherwise = codeAt (n + 1) xs c

-- De functie fromGray zet een graycode c, gegeven een grondtal b, om in de bij-
-- behorende Int, door het 1e voorkomen van die code in de lijst met graycodes op te
-- zoeken en de positie van het voorkomen terug te geven.

fromGray :: Int -> String -> Int
fromGray 0 _ = 0
fromGray 1 _ = 0
fromGray b c = codeAt 0 (generateGray b (fromBase b c)) c

-- De functie toGray zet gegeven een grondtal b, het getal x om in de bijbehorende
-- graycode. De lijst graycodes wordt gegenereerd tot hij groter is dan het getal x,
-- vervolgens worden de 1e x+1 waarden uit die lijst gehaald. De laatste is dan de
-- bijbehorende graycode.

toGray :: Int -> Int -> String
toGray 0 _ = ""
toGray 1 _ = ""
toGray b x = last (take (x + 1) (generateGray b x))

-- De functie grayCode geeft gegeven een grondtal b een tuple functies, die een
-- graycode omzet naar een Int respectievelijk een Int omzet naar graycode.

grayCode :: Int -> ([Char] -> Int, Int -> [Char])
grayCode b = (fromGray b, toGray b)

-- Opdracht 9
-- Werking: Gegeven een integer, waarop een look-and-say reeks wordt gemaakt. Hierbij wordt de functie look herhaalt. En moet de
--          integer een waarde hebben tussen 0 en 9, aangezien de reeks niet goed klopt.
lookAndSay :: Int -> [String]
lookAndSay a | (a < 9) && (a >= 0) = [redoStr c look (showInt a) | c <- [0..]]
             | otherwise = [""]

-- Onderdeel van opdracht 9
-- Werking: Gegeven een integer, een functie en een string. Hierbij wordt een functie een a aantal keer gedaan op de string.     
        
redoStr :: Int -> (String -> String) -> String -> String
redoStr a f g | a > 0 = redoStr (a-1) f (f g)
              | otherwise = g

-- Onderdeel van opdracht 9
-- Werking: Gegeven een string. Hierbij wordt één vervolg element van de look-and-say reeks gegenereert door middel van de lengte
--          van de lijst van de opeenvolgende karakters te pakken en voor de karakter te zetten.

look :: String -> String
look [] = []
look (x:y)    = showInt (lengte) ++ showInt (ord x - 48) ++ look (drop lengte (x:y))
              where lengte = length( takeWhile (==x) (x:y))

-- Onderdeel van opdracht 10
-- Werking: Gegeven een integer en een lijst van integers, hierbij zoekt deze functie de integer in de lijst van integer. Door middel
--          van elk element af te gaan totdat de opvolgende integers in de lijst groter is dan het element gezocht of dat het element is gevonden.
--          Indien een element is gevonden, komt er True uit.

inList :: Int -> [Int] -> Bool
inList _ []     = False
inList a (x:xs) | a > x  = inList a xs
                | a == x = True
                | otherwise = False

-- Onderdeel van opdracht 10
-- Werking: Hierbij wordt een string opgesplitst in een lijst van integers. Hierbij wordt er gebruik gemaakt van dat een string bestaat
--          uit een lijst van karakters.

decompose :: String -> [Int]
decompose []     = []
decompose (x:xs) = ((ord x - 48) : []) ++ decompose xs

-- Onderdeel van opgave 10
-- Werking: Hierbij wordt een lijst gegenereert dat lijkt op een fibonnaci-soort-reeks. Hier wordt gebruik gemaakt om een String om te zetten
--          in een lijst van integers en die aan de opeenvolgende integers vast te plakken.

keithSequenceList :: Int -> [Int]
keithSequenceList a = dec ++ keithSequenceListRest dec (length dec)
                    where dec = decompose (showInt a)

-- Onderdeel van opgave 10
-- Werking: Hierbij wordt een lijst gemaakt van alleen de opeenvolgende integers van de fibonnaci-soort-reeks, door het aantal achterste elementen
--          dat correspondeert met het aantal elementen in het beginstuk wordt opgeteld.

keithSequenceListRest :: [Int] -> Int -> [Int]
keithSequenceListRest a b = s : keithSequenceListRest (drop 1 (a ++ [s])) b
                          where s = sum(take b (reverse a))

-- Onderdeel van opgave 10
-- Werking: Hierbij wordt gekeken of een getal in zijn eigen "keithSequenceList" voorkomt door gebruik te maken van voorgaande functies.

keithInList :: Int -> Int
keithInList a | inList a (keithSequenceListRest dec (length dec)) == True = a
              | otherwise = 0
              where dec = decompose (showInt a)

-- Opdracht 10
-- Werking: Hierbij wordt een lijst gemaakt van de elementen die terugkomen uit keithInList. Hierbij wordt een 0 weggefilterd.

keithGetallen :: [Int]
keithGetallen = filter (/=0) [keithInList x | x <- [10..]]