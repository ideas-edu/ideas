{- 
Functioneel programmeren Practicum nr1.
Sharon Gieske, 3407039
gebruikte compiler = Helium
-}

--Opgave 1 fromDec
{- 
uitleg:
bij het invoer van een lege lijst komt de waarde 0 uit. 
Voor de lijst (x:xs) wordt een recursie aangeroepen. 
Elke x wordt vermenigvuldigt met 10^(lengte van de rest van de tabel). 
voorbeeld: [4,2,3] -> x=4, lengte xs=2 -> 4 * 10^2
dit wordt opgeteld bij de volgende aanroep van fromDec waarbij nu als x 2 wordt gepakt en xs 1.
Wanneer de lijst leeg is, wordt de waarde 0 dus erbij opgeteld. Dit voegt namelijk niets toe aan het getal.
-}
fromDec :: [Int] -> Int
fromDec [] = 0										 
fromDec (x:xs) = x*10^(length xs) + fromDec xs		


--Opgave 2 toDec
{- 
uitleg:
Een ingevoerd getal (stel x) wordt gedeeld door 10 met de functie iterate. 
Hierbij krijg je een oneindige lijst met [x/10, x/10/10, x/10/10/10..] 
Op een gegeven moment wordt x zo vaak gedeeld door 10 dat er (oneindig) staart van nullen in de lijst staat.
Aan deze nullen hebben we niets, dus we pakken uit deze oneindige lijst alle elementen zolang ze niet gelijk zijn aan 0 mbv takeWhile.
Over al de elementen in de lijst nemen we het laatste cijfer. Dit cijfer krijg je dmv de rest bij deling door tien.
De getallen staan in de verkeerde volgorde, dus met reverse draaien we deze getallen weer in de goede volgorde.
-}
toDec :: Int -> [Int]
toDec = reverse.map(`rem`10).takeWhile(/=0).iterate(`div`10)

-- opgave 3 fromBin
{- 
Deze opgave heeft dezelfde basis als opgave 1, hier is het grondgetal echter geen 10, maar 2. 
Overal waar 10 stond in opgave 1 wordt vervangen door 2
-}
fromBin :: [Int] -> Int
fromBin [] = 0
fromBin (x:xs) = x*2^(length xs) + fromBin xs

-- opgave 4 toBin
{- Deze opgave heeft dezelfde basis als opgave 2. Net als bij opgave 3, wordt 10 vervangen door 2 (dit is hier het grondgetal).
-}
toBin :: Int -> [Int]
toBin = reverse.map(`rem`2).takeWhile(/=0).iterate(`div`2)



-- opgave 5 fromBase
{- voor deze functie worden nog twee hulpfuncties gebruikt.
er wordt een lijst Chars meegegeven en een int. Deze int is het grondgetal.
Het is bijna hetzelfde principe als opgave 1.
Wanneer gekeken wordt naar opgave 1 wordt 10 vervangen door n (het grondgetal). 
In plaats van een lijst Ints wordt echter een lijst Chars meegegeven. 
Deze moeten we omzetten door de bijbehorende Int te vinden in de lijst. 
Deze lijst maken we met een hulpfunctie. We zoeken de Char op en de bijbehorende Int wordt meegegeven.
-}

fromBase :: Int -> [Char] -> Int
fromBase _ [] = 0
fromBase n (x:xs) =  (opZoeken x charNaarInt)*n^(length xs) + fromBase n xs

{-
Er is een lijst van tuples nodig om de Chars hun bijpassende Int mee te geven.
Deze wordt gemaakt door de functie zip met als ene lijst alle Chars van 1-9 en a-z en met als andere lijst getallen 0 tot 36.
Het type is dus een lijst van tuples van Chars en Ints
-}
charNaarInt :: [(Char, Int)] 
charNaarInt = zip (['0'..'9']++['a'..'z']) [0..35]

{-
Om de lijst uit de lijst van tuples het behorende tweede deel van de tuple te verkrijgen wordt een functie.
Deze functie bekijkt van elk tuple zijn eerste deel en vergelijkt dit met de meegegeven variabele. 
Wanneer het eerste deel gelijk is aan deze variabele, wordt het tweede deel terug gegeven.
Als dit niet het geval is dan wordt in de rest van de lijst met tuples verder gezocht (mbv recursie)
Er gaat dus een a en een lijst van (a,b)'s in en een b uit. Deze a moet dus wel ordenbaar zijn!
-}
opZoeken :: Eq a => a -> [(a,b)] -> b
opZoeken e ((a,b):ts) 
	| a==e = b
	| otherwise = opZoeken e ts

-- opgave 6
{- Voor deze functie worden twee hulpfuncties gebruikt (Opzoeken en intNaarChar)
er gaan twee getallen in. het eerste, n, is een grondgetal en het tweede is het getal, x,  waarvan de reeks Chars wordt gemaakt.
Het getal van x wordt gedeeld door het grondgetal. 
Bij het restgetal wordt de bijbehorende Char opgezocht met behulp van de hulpfunctie opZoeken. Deze gebruikt de lijst tuples intNaarChar.
Deze Char wordt in een lijst gestopt. Voor het getal wat ontstaat door deling van het grondgetal wordt de functie toBase opnieuw aangeroepen.
Deze functie gaat door tot x 0 is geworden. Bij x = 0 wordt een lege lijst meegegeven.
-}

toBase :: Int -> Int -> [Char]
toBase _ 0 = []
toBase n x = toBase n (x `div` n) ++ [a]
	where a = opZoeken(x `mod` n) intNaarChar
	
{- deze tabel is bijna gelijk aan tabel charNaarInt, alleen zijn de elementen in de tuple omgedraaid
-}	
intNaarChar :: [(Int, Char)] 
intNaarChar = zip [0..35] (['0'..'9']++['a'..'z'])



--
--Opgave 7.
{-
In deze formule gaat een Int (het grondgetal) en een lijst van String.
Voor elke String wordt de functie fromBase aangeroepen. Een string is een lijst Chars, dus dit past er in.
Van deze String wordt dus het bijbehorende getal opgezocht dmv fromBase.
Elke String wordt vervolgens aan een lege lijst toegevoegd en elke getal verkregen dmv fromBase wordt ook toegevoegd op een lege lijst.
Deze lijsten worden gezipt en samengevoegd met de lijst die ontstaat dmv de recursieve aanroep van numbers. 
Zo onstaat de lijst van tuples. Wanneer in een String een Char voorkomt die niet in de gebruikte tabel (gebruikt bij fromBase) zit,
moet de String en het getal niet worden opgeleverd.
Dit wordt gedaan met een if. Als de eerste lijst Chars gelijk is aan de lijst Chars waarbij de oorspronkelijke lijstChars ingevoerd is in een functie van toBase op fromBase.
Hiermee wordt gekeken of alle Chars in de String werkelijk gebruikt worden mbt het grondgetal.
's' wordt bv niet gebruik bij het grondgetal 16.
Zo worden aan Chars die niet bij het grondgetal horen, niet met een getal toegekent. 
Wanneer de Char niet zou moeten worden gebruikt moet een lege lijst opgeleverd worden.
Wanneer de lijst Strings leeg is, moet een lege lijst worden opgeleverd.
-}

numbers :: Int -> [String ] -> [(String, Int)]
numbers _ [] = []
numbers n (x:xs) = 
	if x == toBase n (fromBase n x)
		then zip (x:[]) ((fromBase n x):[]) ++ numbers n xs
	else []
					

					
					
{- Opgave 8. Graycode
grayCode :: Int -> ([Char] -> Int, Int -> [Char])
-niet uitgewerkt
-}



--Opgave 9. Look and Say

lookAndSay :: Int -> [String ]
lookAndSay n = iterate (a) n
	where a = maakLookAndSayReeks 

-- van [int] naar int. De gemaakte lijst dmv maakLookAndSayReek wordt omgezet naar een getal ipv een lijst getallen.
lijstInt :: [Int] -> Int
lijstInt (x:xs) = fromDec c
			where c = maakLookAndSayReeks (x:xs)

-- van een lijst Ints wordt een lijst van lijsten gemaakt waarbij dezelfde Ints bij elkaar in een lijst zitten (dmv span)
-- van deze lijst wordt een lijstje gemaakt bestaand uitde lengte +1 en x, dit wordt samengevoegd met de recursie van de rest van de gemaakte lijst.
maakLookAndSayReeks :: [Int] -> [Int]
maakLookAndSayReeks [] = []
maakLookAndSayReeks (x:xs) = [length a +1,x] ++ maakLookAndSayReeks b
							where (a,b) = span (==x) xs



{-Opgave 10

keithGetallen :: [Int]
keithGetallen :: filter p [x]
-niet uitgewerkt
-}