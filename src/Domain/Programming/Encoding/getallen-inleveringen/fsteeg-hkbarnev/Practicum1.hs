-- FUNCTIONEEL PROGRAMMEREN - PRACTICUM 1
-- Rick Barneveld - Floor van Steeg

module Practicum where
import Char


-- OPDRACHT 1

{-- Als de lijst minimaal 2 integers bevat, wordt het eerste getal met 10 vermenivuldigd 
(decimale stelsel) en vervolgens wordt daar het tweede getal bij opgeteld. Hier wordt de staart 
van de lijst bij aangeplakt en dit er wordt opnieuw het eerste getal vermenigvuldigd met 
10 enz. Totdat de lijst uit één getal bestaat, dan wordt dat getal als output gegeven. --}

fromDec :: [Int] -> Int
fromDec [] = 0
fromDec [s] = s
fromDec (s:t:staart) = fromDec ((10*s+t):staart)


-- OPDRACHT 2

{-- Bij deze functie wordt de input modulo 10 (decimale stelsel) geplakt achter de toDec van de
input gedeeld door 10. Zo wordt het getal dus steeds een factor tien kleiner en wordt de 
"input modulo 10" steeds in de lijst geplakt totdat de input nul is --}

toDec :: Int -> [Int]
toDec 0 = []
toDec a = toDec (a `div` 10) ++ [a `rem` 10]


-- OPDRACHT 3

{-- Als de lijst minimaal 2 integers bevat, wordt het eerste getal met 2 vermenivuldigd 
(binaire stelsel) en vervolgens wordt daar het tweede getal bij opgeteld. Hier wordt de staart 
van de lijst bij aangeplakt en dit er wordt opnieuw het eerste getal vermenigvuldigd met 
2 enz. Totdat de lijst uit één getal bestaat, dan wordt dat getal als output gegeven. --}

fromBin :: [Int] -> Int
fromBin [] = 0
fromBin [s] = s
fromBin (s:t:staart) = fromBin ((2*s+t):staart)


-- OPDRACHT 4

{-- Bij deze functie wordt de input modulo 2 (binaire stelsel) geplakt achter de toDec van de
input gedeeld door 2. Zo wordt het getal dus steeds een factor twee kleiner en wordt de 
"input modulo 2" steeds in de lijst geplakt totdat de input nul is --}

toBin :: Int -> [Int]
toBin 0 = []
toBin a = toBin (a `div` 2) ++ [a `rem` 2]


-- OPDRACHT 5

{-- Eerst wordt de functie charNum gemapt over de input String. "deze functie zorgt 
ervoor dat alle characters worden omgezet naar de daarbij behorende decimale cijfers" 
Vervolgens wordt in de hulpfunctie toNum het grondtal (genaamd base) vermenigvuldigd 
met de string die is meegegeven bij de aanroep van frombase--}

fromBase :: Int -> String -> Int
fromBase base = toNum base . map charNum

charNum :: Char -> Int
charNum a   | b <= ord 'z' && b >= ord 'A' = b - (ord 'A'-10)
            | b <= ord '9' && b >= ord '0' = b - ord '0'
            | otherwise = 0
                where b = ord (toUpper a)

toNum :: Int -> [Int] -> Int
toNum _ [] = 0
toNum _ [s] = s
toNum b (s:t:staart) = toNum b ((b*s+t):staart)

-- OPDRACHT 6
{--Eerst wordt de functie numChar gemapt over de lijst die in de hulpfunctie toBase' wordt
gemaakt met de parameters die bij de aanroep worden meegegeven. In deze hulpfunctie 
wordt een lijst gemaakt door het getal te delen door het grondtal, en dat te concateneren
met de lijst waarin de rest-bij-deling van deze getallen staat. Dit is de uiteindelijke lijst
waarmee numChar aan de slag kan --}

toBase :: Int -> Int -> String
toBase _ 0 = ['0']
toBase base number = map numChar (toBase' base number)

numChar :: Int -> Char
numChar a | a >= 0 && a <= 9 = chr (a + ord '0')
          | a >= 10 && a <= 36 = chr ((a-10) + ord 'a')
          | otherwise = '0'

toBase' :: Int -> Int -> [Int]
toBase' _ 0 = []
toBase' base a = toBase' base (a `div` base) ++ [a `rem` base]


-- OPDRACHT 7
{-- Over de lijst met namen wordt eerst een filter gehaald. Alle namen die niet correct 
zijn volgens het grondgetal worden eruit gehaald. Vervolgens wordt de functie fromBase 
op de namen losgelaten met een map. Dan voegen we de naam (str) samen met zijn waarde 
in een tupel. --}

numbers :: Int -> [String] -> [(String, Int)]
numbers base lst = map (\str -> (str, fromBase base str)) (filter (checkBase base) lst)



checkBase :: Int -> String -> Bool
checkBase _ [] = True
checkBase base (x:xs) | charNum x <= base = checkBase base xs
                      | otherwise = False


-- OPDRACHT 8
{-- De functie grayCode1 is in principe precies het tegenovergestelde van grayCode2. Beide
functies maken van de input een string. Vervolgens wordt elke character langsgelopen, dit
character wordt omgezet naar zijn bijbehorende decimale waarde (dmv charNum) en vervolgens
tellen we daar een tellertje (num) bij op of trekken we het tellertje ervan af. Dit doen we dan
modulo het grondgetal dat is ingevoerd en dan hebben we het eerste nummer van de grayCode.
Het tellertje wordt dan opgehoogd met de uitkomst (bij grayCode2) of wordt vervangen door de
uitkomst (bij grayCode1). Dan roepen we de functie weer recursief aan, zodat het volgende getal
in de lijst wordt afgewerkt. --}
grayCode :: Int -> ([Char] -> Int, Int -> [Char])
grayCode a = (grayCode1 a, grayCode2 a) 


grayCode1 :: Int -> [Char] -> Int
grayCode1 base str = toNum base (map charNum (f2 base 0 str))

f1 :: Int -> Int -> [Char] -> String
f1 _ _ [] = []
f1 base num (x:xs) = (numChar q) : f1 base (q) xs
     where q = ((charNum x) + num)`mod`base



grayCode2 :: Int -> Int -> [Char]
grayCode2 base num = f1 base 0 (toBase base num)

f2 :: Int -> Int -> [Char] -> String
f2 _ _ [] = []
f2 base num (x:xs) = (numChar q) : f2 base (num+q) xs
     where q = ((charNum x) - num)`mod`base



-- OPDRACHT 9
{--Er wordt een lijst gemaakt met oneindig veel Look-And-Say getallen. Deze is natuurlijk niet
echt oneindig maar wordt berekend (dankzij lazy evaluation) tot en met het element waarvan men het 
vraagt. Dus tot en met dat getal wordt de hulpfunctie lookAndSayNext aangeroepen. In de functie 
lookAndSayNext wordt telkens het volgende Look-And-Say getal berekend. Als de head van de lijst 
met getallen gelijk is aan het tweede getal van die lijst, dan doen we het tellertje plus 1 en 
we kijken verder in die lijst. Als dit niet het geval is, dan outputten we het tellertje met het 
daarbij behorende getal. Als er dus in gaat 222 wordt het tellertje verhoogt tot 3 en vervolgens 
wordt dit de output: 32 --}
lookAndSay :: Int -> [String]
lookAndSay start = startStr : (lookAndSayInf (startStr))
     where startStr = toBase 10 start

{-- Deze hulpfunctie creëert een oneindige lijst Look-And-Say getallen --}
lookAndSayInf :: String -> [String]
lookAndSayInf a = b : (lookAndSayInf b)
     where b = lookAndSayNext a 1 ""

{--Deze hulpfunctie berekent één opvolgend Look-And-Say getal --}
lookAndSayNext :: String -> Int -> String -> String
lookAndSayNext (a:[]) b c = (c ++ [numChar b] ++ [a])
lookAndSayNext a b c      | head a == head (tail a) = lookAndSayNext (tail a) (b+1) c
                          | otherwise = lookAndSayNext (tail a) 1 (c ++ [numChar b] ++ [head a])



-- OPDRACHT 10
{-- De functie begint met het getal 10 en probeert vervolgens elk getal uit of het een Keith getal is.
Of iets een Keith getal is wordt gecheckt in de functie testKeith. In keithGetal wordt de invoer
omgezet naar een lijst met integers. Deze lijst wordt vervolgens in testKeith gecheckt: als de
laatste van die lijst kleiner is dan het getal dat we als invoer hebben meegegeven (a) dan
moeten we even verder rekenen (dus de sum van de lijst achter de tail van de lijst plakken).
Als de invoer gelijk is aan het laatste getal van de lijst, dan hebben we een Keith getal
gevonden en die geven we dan terug aan keithGetal, die vervolgens dit getal in een lijstje zet.
Als echter het laatste getal van de lijst hoger is dan het invoer getal, dan zal er de invoer
ook nooit meer een Keith getal worden, dus dan geven we 0 door aan keithGetal. Zo maakt
keithGetal dus een lijstje van nullen en Keith getallen. Dit lijstje filteren we dan op alles
wat niet nul is, en daaruit komen dus enkel de echte Keith getallen te voorschijn --} 
keithGetallen :: [Int]
keithGetallen = filter (/=0) (keithGetal 10)

keithGetal :: Int -> [Int]
keithGetal a = (f3 a (toDec a)): keithGetal (a+1)

f3 :: Int -> [Int] -> Int
f3 a (x:xs) | a > (lst) =  f3 a (xs ++ [sum (x:xs)])
            | a == (lst) = lst
            | a < (lst) = 0
                 where lst = last xs