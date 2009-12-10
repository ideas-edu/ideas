{-	    Practicum Opdracht 1.
		Door: Pepijn Kokke (3377520) en Bart Liefers (3021041)
		Gebruikte compilers: GHC, Helium (en WinHugs voor de reductions en cells)      -}

-- Opgave 1. Schrijf een functie "fromDec :: [Int] -> Int" die voor een reeks decimale cijfers de bijbehorende interpretatie als som van 10-machten produceert.
fromDec :: [Int] -> Int
fromDec [x]      = x
fromDec (x:y:ys) = fromDec (x * 10 + y : ys)
{- Een lijst met een enkele Int-waarde geeft gewoon deze int terug, maar als er twee of meer waardes in een lijst zitten, 
   geef dan de lijst waarbij de eerste waarde weggelaten is, en tien maal opgeteld is bij de tweede.
   De functie fromDec kan ook in een enkele regel gedefinieerd worden als volgt, echter, dan is hij minder efficient,
   namelijk 54 reductions en 66 cells voor de standaardbewerking fromDec [4,2,3] -> 423.
   
   fromDec = foldl (\x y -> x * 10 + y) 0 
   -}
-- fromDec [4,2,3] -> 423 (41 reductions, 52 cells)

-- Opgave 2. Schrijf een functie "toDec :: Int -> [Int]" die een getal omzet naar de bijbehorende decimale cijferreeks.
toDec :: Int -> [Int]
toDec x | x < 10    = [x]
        | otherwise = toDec (div x 10) ++ [mod x 10]
{- Waarden kleiner dan 10 kunnen direct in een lijst gestopt worden, in andere gevallen pakken we het laatste getal,
   door x 'mod' 10 te berekenen, en plakken we de rest ervoor, waarbij alles door 10 gedeeld wordt. -}
-- toDec 423 -> [4,2,3] (97 reductions, 143 cells)

-- Opgave 3. Schrijf een functie "fromBin :: [Int] -> Int" die voor een reeks binaire cijfers de bijbehorende interpretatie als som van 2-machten produceert.
fromBin :: [Int] -> Int
fromBin [x]      = x
fromBin (x:y:ys) = fromBin (x * 2 + y : ys)
{- Vrijwel identiek aan Opgave 1, het enige verschil is dat we nu twee maal,
   in plaats van tien maal de eerste Int-waarde bij de volgende optellen.
   Voor fromBin geldt hetzelfde als fromDec, het kan ook in een enkele regel gedefinieerd
   worden als volgt, echter, dan is hij minder efficient, namelijk 84 reductions en 95 cells 
   voor de standaartbewerking fromBin [1,0,0,1,1,0] -> 38.
   
   fromBin = foldl (\x y -> x * 2 + y) 0   
   -}
-- fromBin [1,0,0,1,1,0] -> 38 (68 reductions, 81 cells)

-- Opgave 4. Schrijf een functie "toBin :: Int -> [Int]" die een getal omzet naar de bijbehorende binaire cijferreeks.
toBin :: Int -> [Int]
toBin x | x < 2     = [x]
        | otherwise = toBin (div x 2) ++ [mod x 2]
{- Vrijwel identiek aan Opgave 2, het enige verschil is dat de grenzen nu bij 2 liggen,
   in plaats van bij tien. 
   -}
-- toBin 38 -> [1,0,0,1,1,0] (187 reductions, 290 cells)

-- Opgave 5. Schrijf een functie "fromBase :: Int -> [Char] -> Int" die voor een grondtal tussen 2 en 36 (inclusief) en een cijferreeks de bijbehorende interpretatie produceert.
fromBase :: Int -> [Char] -> Int
fromBase base list = fromBase' (map digitToInt list)
    where
        fromBase' :: [Int] -> Int
        fromBase' [x] = x
        fromBase' (x:y:ys) = fromBase' ((x * base + y) : ys)
digitToInt :: Char -> Int
digitToInt x = b - a
    where  b = fromEnum x
           a = if b > 86 then 87 else 48
{- We hebben hier twee hulpfuncties gedefinieerd. Allereerst is het handig om de lijst van Char-waardes om te
   zetten naar een lijst van Int-waardes. Dit kan met behulp van de functie 'digitToInt', waar de waarde van Char-waardes wordt omgezet
   naar een Int, zodat '0' afbeelt op 0, 'a' afbeelt op 10, en 'z' afbeelt op 35. Als dit gebeurd is kunnen we dezelfde
   methode gebruiken als bij opgave 1 en 3.
   Voor fromBase geldt hetzelfde als voor fromDec en fromBin, het kan ook in een enkele regel gedefineerd
   worden als volgt, echter, dan is hij minder efficient, namelijk 98 reductions en 140 cells 
   voor de standaartbewerking fromBase 10 "423" -> 423. Daarbij blijft één hulpfunctie noodzakelijk.
   
   fromBase base = (foldl (\x y -> x * base + y) 0).(map digitToInt) 
   -}
-- fromBase 10 "423" -> 423 (87 reductions, 127 cells)

-- Opgave 6. Schrijf een functie "toBase :: Int -> Int -> [Char]" die voor een grondtal tussen 2 en 36 (inclusief) en een getal de bijbehorende cijferreeks produceert.
toBase :: Int -> Int -> [Char]
toBase _ 0         = []
toBase base number = (toBase base (div number base)) ++ [intToDigit (mod number base)]
intToDigit :: Int -> Char
intToDigit x 
    | x < 10    = toEnum (x + 48)
    | otherwise = toEnum (x + 87)
{- Deze uitwerking lijkt heel veel op die van Opgave vier en twee,
   we hebben nu ook de hulpfunctie 'intToDigit' nodig, om de Int-waardes
   om te zetten naar Char-waardes (0 naar '0', 10 naar 'a', 35 naar 'z').
   -}
-- toBase 10 423 -> "423" (174 reductions, 230 cells)

-- Opgave 7. Schrijf een functie "numbers :: Int -> [String ] -> [(String, Int)] die voor een grondtal tussen 2 en 36 (inclusief) en een lijst met woorden de woorden produceert die overeenkomen met een getal.
numbers :: Int -> [String] -> [(String, Int)]
numbers base list = [(c , fromBase base c) | c<-list , inRange c]
    where
        inRange :: [Char] -> Bool
        inRange []    = True
        inRange (x:xs)= (base > digitToInt x) && inRange xs
{- We maken hier gebruik van de functie fromBase uit Opgave vijf.
   Verder hebben we de hulpfunctie 'inRange' gedefinieerd, die
   kijkt of er geen letters zijn die geen waarde representeren
   voor dit grondtal. 
   -}
-- numbers 16 ["ace", "face", "faces"] -> [("ace",2766),("face",64206)] (628 reductions, 912 cells)

-- Opgave 8. Schrijf een (efficiente) functie "grayCode :: Int -> ([Char] -> Int, Int -> [Char])" die voor een gegeven aantal cijfers (grondtal) tussen 2 en 36 (inclusief) een Gray-codering construeert, dat is, een tweetal functies waarvan de eerste een cijferreeks interpreteert als een natuurlijk getal en de tweede voor een willekeuring natuurlijk getal de bijbehorende cijferreeks produceert. (De codering dient daadwerkelijk gebruik te maken van alle beschikbare cijfers.)
grayCode :: Int -> ([Char] -> Int, Int -> [Char])
grayCode n = (grayFromString n, grayFromInt n)
grayFromInt :: Int -> Int -> String
grayFromInt n x
    | x < n     = [intToDigit x]
    | otherwise = grayFromInt n (div x n) ++ [intToDigit (fmod n x)]
    where
        fmod :: Int -> Int -> Int
        fmod n x
            | a < n     = a
            | otherwise = b - a -1
            where
                a = rem x b
                b = 2 * n
grayFromString :: Int -> String -> Int
grayFromString n lijst = grayFromString' n (map digitToInt lijst)
    where
        grayFromString' :: Int -> [Int] -> Int
        grayFromString' _ [x] = x
        grayFromString' n (x:xs)
            | even x    = x*r + (grayFromString' n xs) 
            | otherwise = x*r + (inv r (grayFromString' n xs)) 
            where
                inv :: Int -> Int -> Int
                inv n x = n - x -1
                r       = n ^ (length xs)
{- Voor de grayCode hebben we een aantal hulpfuncties gedefinieerd. 
   'grayFromInt' doet het volgende: als hij wordt aangeroepen met een getal
   kleiner dan het grondtal, dan zal gewoon dit getal gebruikt worden (met
   gebruik van de eerder geschreven 'intToDigit'). In alle andere gevallen komt er
   een getal voor te staan, en moet het getal zelf alternerend van 0 naar n,
   en weer terug lopen. De functie fmod zorgt ervoor dat dit gebeurt. Het getal
   dat er voor komt te staan kan op dezelfde manier iteratief worden bepaald,
   waarbij de aanroep gebeurt op x 'div' n in plaats van x.

   'grayFromString' zet allereerst de String om in een lijst van Ints, om 
   dezelfde reden als bij opgave 5. Vervolgens wordt van deze lijst van
   Int-waardes gekeken welke code hierbij hoort. Het eerste getal van de gray-code
   geeft de basis van de waarde, op een soort gelijke manier als in een binair
   systeem. De waarde kan dus met de macht van de kolom (lengte van de code-1) 
   vermenigvuldigd worden. Omdat in grayCode, de waarden na de eerste alterneren
   tussen oplopen van 0 naar n en vice versa, kunnen de waarden van de volgende
   kolom op twee manieren worden geinterpreteerd. In het geval van een even start
   getal werkt het als in binair: de volgende waarden worden er bijopgeteld. Als
   het startgetal oneven is, gebruiken we de functie 'inv' om het afbouwende geval
   van het alterneren te ondervangen. 
   -}
-- map (snd (grayCode 2)) [1..7] -> ["1","11","10","110","111","101","100"] (1222 reductions, 1624 cells)

-- Opgave 9. Schrijf een functie "lookAndSay :: Int -> [String]" die gegeven een eerste element de bijbehorende (oneindige) look-and-say-reeks produceert.
lookAndSay :: Int -> [String]
lookAndSay x = lasReeks (toBase 10 x)
    where
        lasReeks :: String -> [String]
        lasReeks list = list : (lasReeks(next list))
            where
                next :: String -> String
                next [] = []
                next y  = (intToDigit aantInY) : (head y) : (next (drop aantInY y))
                    where
                        aantInY = aantal y
                        aantal :: String -> Int
                        aantal [_] = 1
                        aantal (a:b:cs)
                            | a == b    = 1 + aantal (b:cs)
                            | otherwise = 1
{- Om de functie efficienter te maken hebben we aangenomen dat lookAndSay 
   niet wordt aangeroepen met meer dan 9 dezelfde opeenvolgende elementen. 
   Er is uberhaubt maar 1 waarde (1111111111) waarbij het geen stack-overflow
   oplevert, en in de rest van de reeks komen verder nooit getallen hoger dan
   3 voor. 
   De eerste lookAndSay roept serie aan. Serie zet de huidige reeks voor de
   volgende (Met behulp van 'next'). 'next' berekent de String die volgt
   op de aanroep. Dit houdt in: het aantal (y, via de functie aantal) dezelfde
   tekens op kop, voor dit teken zelf (head x). Dit wordt recursief herhaald
   tot de hele String is doorlopen. 
   -}
-- take 6 (lookAndSay 1) -> ["1","11","21","1211","111221","312211"] (1228 reductions, 1569 cells)

-- Opgave 10. Construeer een (oneindige) lijst "keithGetallen :: [Int]" die alle Keith-getallen in oplopende volgorde bevat.
keithGetallen :: [Integer]
keithGetallen = filter iskg [10..]
    where
        iskg :: Integer -> Bool
        iskg x = element x (keithReeks (toDec' x))
            where
                -- Een herdefinitie van toDec zodat hij werkt voor Integer waardes.
                toDec' :: Integer -> [Integer]
                toDec' x
                    | x < 10    = [x]
                    | otherwise = toDec' (div x 10) ++ [rem x 10]
                element :: Integer -> [Integer] -> Bool
                element x (y:ys)
                    | x > y     = element x ys
                    | x == y    = True
                    | otherwise = False
                keithReeks :: [Integer] -> [Integer]
                keithReeks lijst = head lijst : keithReeks (nextKeith lijst)
                    where
                        nextKeith :: [Integer] -> [Integer]
                        nextKeith xs = (tail xs) ++ [sum xs]
{- De functie keithGetallen kijkt voor alle getallen groter dan 14 (het eerste keith-getal of ze 
   een keithGetal zijn (via iskg). iskg kijkt of een getal een 'element' is
   uit zijn eigen lijst via 'element'. We kunnen hier niet de ingebouwde functie
   elem gebruiken, omdat de 'keithReeks' oneindig is, en we moeten stoppen als 
   we aanbelanden bij een getal dat groter is dan het getal waarmee de reeks 
   geinitialiseerd werd. 'keithReeks' bouwt de reeks op via 'nextKeith', die
   van een lijst (met lengte n van het initele getal) de som bepaalt, en deze
   achter de voorgaande reeks plakt. Door de tail te nemen blijft de lengte
   van de lijst hetzelfde, waardoor het voor de volgende aanroep ook weer goed
   gaat. Verder hebben we Opdracht 2, de functie 'toDec' opnieuw gedefineerd om te werken voor Integer-waardes. 
   -}
-- take 5 keithGetallen -> [14,19,28,47,61] (12154 reductions, 18946 cells) of (157 reductions, 224 cells) wanneer take x keithGetallen, waarbij (x >= 5) al berekend is.

{-
 1> fromDec [4,2,3]						-> 423 											(41 reductions, 52 cells)
 2> toDec 423 							-> [4,2,3]										(97 reductions, 143 cells)
 3> fromBin [1,0,0,1,1,0] 				-> 38 											(68 reductions, 81 cells)
 4> toBin 38 							-> [1,0,0,1,1,0] 								(187 reductions, 290 cells)
 5> fromBase 10 "423" 					-> 423 											(87 reductions, 127 cells)
 6> toBase 10 423 						-> "423" 										(174 reductions, 230 cells)		
 7> numbers 16 ["ace", "face", "faces"] -> [("ace",2766),("face",64206)] 				(628 reductions, 912 cells)
 8> map (snd (grayCode 2)) [1..7] 		-> ["1","11","10","110","111","101","100"] 		(1222 reductions, 1624 cells)
 9> take 6 (lookAndSay 1) 				-> ["1","11","21","1211","111221","312211"] 	(1228 reductions, 1569 cells)
10> take 5 keithGetallen 				-> [14,19,28,47,61] 							(12154 reductions, 18946 cells)
-}