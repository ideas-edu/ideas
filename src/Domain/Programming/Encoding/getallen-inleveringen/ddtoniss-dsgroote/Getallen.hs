{-
Eerste inleveropgave Functioneel programmeren van Dennis Grootendorst (3117316) en Denise Tönissen (3251535)
Gemaakt en compileert met GHCI compiler versie 6.10.1 
-}

module Getallen where

--opgave 1
fromDec :: [Int] -> Int
fromDec [] = error "Empty list"
fromDec x = (read.(concatMap show)) x

{- ** commentaar opgave 1 **

We hebben hier de wiskundige versie van deze functie die gesuggereerd werd in de opgave niet gebruikt omdat
deze versie korter is en correcter is, want een lege lijst wordt hier niet als 0 beschouwd.

Onze functie maakt van elke int in de lijst een string en plakt deze aan elkaar met concatMap show, resulterende 
in een string bevattende alle ints. Vervolgens door read toe te passen, wordt deze geparsed naar het gewenste resultaat:
een int.

Om te laten zien dat wij deze opdracht ook kunnen maken in de gesuggereerde vorm hebben wij hem er in het commentaar
bij gezet als alternatief:
fromDec2 :: [Int] -> Int
fromDec2 [] = 0
fromDec2 (x : xs)  = x * 10^length(xs) + fromDec2(xs) 

-}

--opgave 2
toDec :: Int -> [Int]
toDec x = map (read.(\x -> [x])) (show x)


{- ** commentaar opgave 2 **

Ook hier hebben we niet voor de wiskundige versie gekozen omdat dit korter is en efficienter (met name voor de grotere getallen).

Deze functie converteert de int naar een string en wordt meegegeven aan map. Map voert een functie uit op elke element
van deze lijst van Char: stop elk element in een lijst en lees hem uit als int.

Om te laten zien dat we ook deze functie wiskundig kunnen oplossen geven we deze als alternatief:
toDec2 :: Int -> [Int]
toDec2 x | x <=9 = [x]
         | otherwise = (toDec (x `div` 10)) ++ [x `rem` 10] -}
         


--opgave 3
fromBin :: [Int] -> Int
fromBin [] = 0
fromBin (h:hs) =h*2^length hs +fromBin hs

{- ** commentaar opgave 3 **

Binaire getallen hebben een grondgetal van 2, beginnend bij 2^0. Daarom door de (lengte van de lijst - 1) te gebruiken
als macht van het grond getal kan zo het decimale getal berekend worden.

-}

--opgave 4
toBin :: Int -> [Int]
toBin 0 = []
toBin x = (toBin (x `div` 2))++[x `mod` 2]

{- ** commentaar opgave 4 **

Deze functie zet een decimaal getal om naar een binair getal enkel in een lijst. Ook hier geldt weer dat binaire
getallen een grondgetal hebben van 2 en dat je rekenen moet houden dat de machten beginnen bij 0. Als je het x-de
binaire getal wilt weten, moet je het getal x-1 keer delen door 2. Vervolgens door de mod ervan te nemen sluit je
uit dat er een groter x is, die deze x overbodig maakt als het ware; i.e. voorkom 0111 voor de waarde 8.

-}

--opgave 5
fromBase :: Int -> [Char] -> Int
fromBase _ [] = 0
fromBase base (h:hs) = (realnumber*(base^length hs)) + fromBase base hs
         where realnumber = parsePowers base h

parsePowers :: Int -> Char -> Int
parsePowers base x | x >= '0' && x <= '9' = read (x:"")
                   | x >= 'a' && x <= 'z'  && base >= 10 && (10+diff) <= base = 10 + diff
                   | otherwise = error "Wrong input"
                       where diff = fromEnum x - fromEnum 'a'
                       
{- ** commentaar opgave 5 **

Deze functie is essentieel hetzelfde als fromBin echter met 2 veranderingen:
1. De lijst bevat letters die omgezet moeten worden naar een cijfer-waarde, om in de vermenigvuldiging te kunnen gebruiken. 
   Hier voor wordt de functie parsePowers gebruikt.
2. Het grondgetal 2 voor binaire getallen is vervangen door een variabele mee te geven base

ParsePowers kijkt of de meegegeven char een integer is door gebruik te maken van de ordening van chars en retournert de integer 
in de vorm van een int als dit het geval is. Zo niet, wordt er gekeken of de char een letter is tussen de a en z (lower cases),
de base groter (of gelijk) is aan 10; 'a' is namelijk het 10de 'getal', en of de letter niet groter is dan de base. Als deze 3
waar zijn, wordt de cijfer voor de bijbehorende letter berekend uit het verschil tussen de letter en de 'a', door gebruik te maken 
van hun ordening en fromEnum. 
Als geen van het bovenstaande het geval is, zijn er gegevens in gegeven die incorrect zijn en wordt een error gegooid.

-}

--opgave 6
toBase :: Int -> Int -> [Char]
toBase _ 0 = []
toBase base x = (toBase base (x `div` base))++(parsePowers' base (x `mod` base))

parsePowers' :: Int -> Int -> [Char]
parsePowers' base x | x >= 0 && x <= 9 = show x
                    | x <= base && base >= 10 && base <=36 = toEnum((x-10) + fromEnum 'a'):""
                    | otherwise = error "Wrong input"

{- ** commentaar opgave 6 **

Deze functie is essentieel hetzelfde als toBin echter met 2 veranderingen:
1. Het cijfer moet nu ook omgezet worden naar letters bij een base groter dan 9. Hier voor wordt de functie parsePowers' gebruikt.
2. Het grondgetal 2 voor binaire getallen is vervangen door een variabele mee te geven base

ParsePowers' zet de getallen 0 tot 9 om van een integer met behulp van show naar een character. Voor de getallen boven de 9 wordt er
eerst gecheckt of het getal kleiner of gelijk aan de base is en of de base wel tussen de 10 en 36 zit. Vervolgens word er uitgerekent
hoeveel het getal boven de 10 zit en daarbij de waarde van rromEnum 'a' opgeteld. En die waarde wordt met toEnum omgezet
in een character, en vastgeplakt aan een lege string. Dit werkt omdat de letters van het alfabet in fromEnum een steeds met 1 oplopende character
waarde hebben vanwege de ordening die ervoor is.
Als geen van het bovenstaande het geval is, zijn er gegevens in gegeven die incorrect zijn en wordt een error gegooid.

-}

--opgave 7
numbers :: Int -> [String] -> [(String,Int)]
numbers _ [] = []
numbers base (x:xs) | base < 10 = error "Base is too low."
                    | base > 36 = error "Base is too high."
                    | any (\y -> (9 + (fromEnum y - fromEnum 'a' )) >= base) x = numbers base xs
                    | otherwise = (x,fromBase base x):numbers base xs
                    
{- ** commentaar opgave 8 **

Al hoewel aangegeven in de opdracht dat deze functie moet werken voor een base tussen 2 en 36, heeft het geen nut een base te nemen
die ligt tussen 2 en 9 (beide inclusief), aangezien de letters van woorden pas geconverteerd worden bij een base van 10. 
Woorden bestaan niet uit cijfers. Daarom wordt er een error gegooid als base kleiner is dan 10. Ook als de base boven de 36 is, wordt 
er een error gegooid.

Als de base wel goed is wordt er gekeken in de string of deze een Char bevat, die bij een int waarde hoort hoger dan de base. Dit wordt gedaan 
door middel van de functie any die gegeven een functie (a->bool) kijkt of er een element in een lijst is die voldoet aan deze functie. Zo ja, wordt
dit element niet toegevoegd aan de lijst, en wordt recursief gekeken naar de volgende string in de lijst.

Als de base wel goed is en de string ook, wordt er een tupel gemaakt, met als eerste element de originele string en als twee element de string
geconverteert naar int door middel van de eerder geschreven fromBase.

Alle bovenstaande 4 geval onderscheiden worden gedaan door middel van guards.

-}

--opgave 8
grayCode :: Int -> ( [Char] -> Int, Int -> [Char] )
grayCode base = (grayCodeFunc1 base, grayCodeFunc2 base)

grayCodeFunc1 :: Int -> [Char] -> Int
grayCodeFunc1 base graycode = until ((==graycode).(grayCodeFunc2 base)) (+1) 0

grayCodeFunc2 :: Int -> Int -> [Char]
grayCodeFunc2 base item = (show.fromDec) ( (!!) (until ((>item).length) (vergroot base 0) startList) item   )
                              where startList = (map toDec (take base (iterate (+1) 0)))


--start functies voor grayCodeFunc 2

--maakt een nieuwe rij van graycode getallen, door een element ervoor te plakken
vergroot :: Int -> Int -> [[Int]] -> [[Int]]
vergroot base count list | base == count = []
                         | otherwise = (map (count:) list) ++ (vergroot base (count+1) rotatedList)
                             where rotatedList = rotateList base (vulAan list)

--roteert het eerste element van een lijst van getallen
rotateList :: Int -> [[Int]] -> [[Int]]
rotateList base list = map (\x -> ((base + (head x) - 1) `mod` base):tail x) list

--voegt nullen toe aan het begin van lijsten die kleiner zijn de grootste lijst van lijsten
vulAan :: [[Int]] -> [[Int]]
vulAan y = map (\x -> (replicate (maxLength - length x) 0)++x) y
            where maxLength = maximum (map length y)

--eind functies voor grayCodeFunc 2

{- ** commentaar opgave 8 **

Graycode wordt gegenereert door het eerste getal van ieder getal te roteren (en niet mirroren zoals het voorbeeld in de opgave suggereerd)
en bij elke rotatie het getal dat er voor bij komt te staan, 1 op te hogen.

Omdat bij rotatie je de 0 ook wilt mee roteren voor getallen, moet deze eerste aangevult worden met nullen, zodat alle lijsten van getallen 
gelijk zijn qua lengte. Dit wordt gedaan met de functie 'vulAan'. Deze functie pakt de maximale lengte van de lijsten en repliceert bij elke lijst
0en toe in de hoeveelheid (maximalelengte - eigenlengte). Dit zijn precies de hoeveelheid 0en die missen.

Vervolgens worden de eerste elementen geroteert. Dit wordt gedaan door van elk eerste element 1 af te trekken en te zorgen, dat -1 gemapped
wordt naar base -1. (d.m.v. (base + (head x) -1) `mod` base). Deze wordt dan weer vastgeplakt aan de originele cijfers van de lijst. Dit genereert 
een rotatie effect van de eerste getallen.

Nu door voor een getal van 0 tot base, voor elke element van de rotatie dit getal te zetten, wordt een rij van graycode getallen geproduceerd. 
Dit wordt gedaan met de functie vergroot.

Door dit proces te herhalen totdat de lijst van graycodes minstens de lengte heeft van het nummer dat je zoekt met until, kan zo het 
benodigde element de !! operator uit de lijst gepakt worden en juist geconverteerd worden van een lijst van int naar int. De lijst waarmee dit 
proces begint wordt gegenereert door een iteratie van +1, en het pakken van base elementen uit de lijst met take. (Lang leve lazy evaluation). 
Iteratie begint bij 0. Nu hebben we graycodefunc2. 

graycodefunc1 is eigenlijk voor elk getal vanaf 0 kijken met behulp van graycodefunc2, of dat getal bij dezelfde waarde hoort, die graycodefunc 
genereert met dit getal. Totdat (until) dat deze waarde gevonden is, wordt dit wordt gezet. Als het getal, de juiste waarde heeft genereert, 
is het juiste getal gevonden en geven we dat getal als resultaat terug.

Om de gecreerde functie te testen van opgave 8 zijn de volgende functies gebruikt. testEerste test uiteraard de eerste
functie en testTweede de tweede functie.

testEerste :: Int -> [Char] -> Int
testEerste base charsss = fst(grayCode base) charsss

testTweede :: Int -> Int -> [Char]
testTweede base intsss = snd(grayCode base) intsss

-}

--opgave 9
lookAndSay :: Int -> [String]
lookAndSay x = iterate lookAndSay' (show x)

lookAndSay' :: String -> String
lookAndSay' "" = ""
lookAndSay' x = amount:firstElement:(lookAndSay' newString)
                  where newString = dropWhile (==firstElement) x
                        amount = (head.show.length) (takeWhile (==firstElement) x)
                        firstElement = head (take 1 x)
                        
{- ** commentaar opgave 9 **

lookAndSay' berekent de lookandsay bij een andere lookandsay. Deze functie pakt het eerste element en gebruikt dan takeWhile om alle
elementen voorop uit de lijst die hetzelfde zijn als dit eerste element eruit te halen. Hier wordt de lengte van bepaald, welke het 
aantal keer het eerste element is. Deze wordt voorop aan het eerste element gezet, om vervolgens dit proces weer recursief te herhalen
door de rest van de lijst met dropWhile door te geven aan de functie weer.

Nu door te itereren over deze lookAndSay', wordt steeds een nieuwe lookandsay toegevoegd aan de lijst, waardoor je een oneindige lijst van
lookandsays krijgt, waarvan je dus door middel van take x, een x aantal lookandsays kan pakken. Handig toch lazy evaluation.

-}

--opgave 10
keithGetallen :: [Int]
keithGetallen = filter isKeithGetal [10..]

isKeithGetal :: Int -> Bool
isKeithGetal getal = elem getal (takeWhile (<=getal) (keithReeks (toDec getal)))

keithReeks :: [Int] -> [Int]
keithReeks (a:as) = a:keithReeks(as++[newKeith])
                           where newKeith = foldr (+) 0 (a:as)

{- ** commentaar opgave 10 **

Eerst wordt een keithreeks produceerd met keithReeks door het eerste getal voorop te zetten aan een lijst, en daarachter 
een nieuwe functie aanroep van dezelfde functie met als parameter de concatenatie van de overige getallen en initiele getallen opgeteld als een lijst.
Door dit steeds recursief steeds te herhalen, zonder basis geval, creeer je zo een oneindelijke lijst van optelling van getallen.

Nu wil je kijken of het getal waaruit de keithReeks is ontstaan, een keithGetal is: de functie isKeithGetal. Hij pakt elementen uit de lijst, tot 
dat het getal zelf is bereikt. Als het getal dan in deze lijst zit, is het een keithgetal.

Als laatste door de functie isKeithGetal te gebruiken in een filter, op een oneindige lijst van integers, steeds 1 groter wordend, kan een oneindige 
lijst met keithgetallen geproduceerd worden.
Er wordt gekeken vanaf het getal 10, omdat getallen onder de 10 alleen maar resulteren in een replicerende lijst van hetzelfde getal.

-}