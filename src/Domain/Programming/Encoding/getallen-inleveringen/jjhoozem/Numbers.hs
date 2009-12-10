module Numbers where

{-
fromDec neemt een lijst van getallen en geeft die terug als een getal
-}
fromDec :: [Int] -> Int
fromDec [] = 0                                  --basisgeval
fromDec (a:b) = let macht = 10^(length (b))     --berekent de waarde van elk getal
                in a * (macht) + fromDec b      --recursief optellen van alle getallen

{-
toDec doet het omgekeerde van fromDec. Er wordt een lijst van 10machten gemaakt met baselist, die voortdurend met het getal vergeleken wordt. Hoe vaak elk getal er in past, 
totdat dat niet meer het geval is. Daarna worden alle verschillen die over zouden blijven 
berekend met map, waardoor je de omgekeerde lijst krijgt. Die wordt dan omgedraaid. 
-}
toDec :: Int -> [Int]
toDec a = reverse (map (`rem` 10) (takeWhile (>0) (map (quot a) (baselist 10))))

{-
fromBin geeft een getal terug met als invoer een binair getal in lijstvorm.
dat gebeurt door steeds de 0 of 1 te vermenigvuldigen met de 2macht de overeenkomt met
de locatie van die digit in de lijst. Dat gebeurt aan de hand van de lengte van de lijst.
de uitkomsten worden opgeteld.
-}
fromBin :: [Int] -> Int
fromBin [] = 0                                  
fromBin (a:b) = let macht = 2^(length (b))   
                in a * (macht) + fromBin b

{-
Eerste versie van toBin; kijkt of de invoer 0 is, 
of het getal oneven is (om er dan een 1 aan te plakken) 
en zet dan een recursieve functie in die voortduren met een grotere 2macht 
berekent of de waarde er in zit. de uitvoer wordt uiteindelijk omgedraaid, 
omdat hij van klein naar groot is opgebouwd.
-}
	
toBin :: Int -> [Int]
toBin 0 = [0]
toBin a = reverse (r : recToBin (a-r) 2)
 where recToBin 0 _ = []
       recToBin x y =  rem (quot x y) 2 : recToBin (x-(y*(rem (quot x y) 2))) (2*y)
       r = rem a 2        

{-
Mooiere versie van toBin: binlist maakt een oneindige lijst van 2machten, 
1-2-4-8-16-enz. die gebruikt toBin2 om haar invoer te vergelijken.
dat gebeurt door eerst quot a op de oneindige baselist te mappen, en vervolgens daar de verschillen (rem) op te mappen. Daar komt voor elk getal in de baselist een 0 of een 1 uit.
dat is het binaire getal.
-}

toBin2 :: Int -> [Int]
toBin2 a = reverse (map (`rem` 2) (takeWhile (>0) (map (quot a) (baselist 2))))

{- 
Oneindige lijst van machten van de invoer.
-}
baselist :: Int -> [Int]
baselist a = iterate (a*) 1

{-
charToInt geeft voor een karakter de bijbehorende waarde in integer terug. dus 0 tot z als character wordt omgezet in een bijbehorende 0-36 integer. Dat gebeurt met de preludefunctie fromEnum die characters omzet in een integer van 0-255. Aangezien de waardes van 0-9 en a-z niet aaneengesloten achter elkaar liggen is deze conversie nodig.
-}
charToInt :: Char -> Int
charToInt c
  | c >= '0' && c <= '9'  =  fromEnum c - fromEnum '0'
  | c >= 'a' && c <= 'z' =  fromEnum c - fromEnum 'a' + 10
 
{-
intToChar doet het omgekeerde van charToInt.
-}
intToChar :: Int -> Char
intToChar a 
 | a > 9  = toEnum ((fromEnum 'a') + (a-10))::Char
 | a <= 9 = toEnum ((fromEnum '0') + a)::Char

{-
fromBase geeft een 10-base getal terug voor een getalsysteem met bijbehorende string als invoer. Het getalsysteem (1e parameter) wordt gebruikt om de getallen uit de lijst telkens met de goede waarde te vermenigvuldigen (macht 'subroutine'). Alle waardes worden dan bij elkaar opgeteld.
-}
fromBase :: Int -> [Char] -> Int
fromBase _ [] = 0                                  
fromBase 0 _ = 0
fromBase x (y:z) = let macht = x^(length (z))     
                       a = charToInt y
                   in a * (macht) + fromBase x z


{-
toBase doet het omgekeerde van fromBase. Dit gebeurt met een methode zoals bij toBin2; map quot op de baselist van het opgegeven getalsysteem totdat er 0 uit komt (dan past het getal er niet meer in), neem daar de rem 's van en zet het omgekeerde in een lijst characters (string)
-}
toBase :: Int -> Int -> [Char]
toBase _ 0 = ['0']
toBase 0 _ = ['0']
toBase a b = map intToChar (reverse (map (`rem` a) (takeWhile (>0) (map (quot b) (baselist a)))))

{-
numbers kijkt of een opgegeven sequentie van waardes in het opgegeven getalsysteem voorkomen.
vervolgens wordt de 10-base waarde samen met de xx-base waarde in een tupel uitgevoerd.
de methode is: check met map, chartoint en >= of het getal in het opgegeven getalsysteem voor komt, als dat zo is zet het dan in een tupel met de waarde en roep recursief de functie weer aan. Als het getal niet in het getalsysteem voorkomt, laat dan de uitvoer achterwege en ga door met de recursie.
-}
numbers :: Int -> [String] -> [(String,Int)]
numbers 0 _ = []
numbers _ [] = []
numbers a (b:c)
 | numberCheck b = (b,(fromBase a b)):numbers a c
 | otherwise = numbers a c 
 where  numberCheck x = not(elem False (map (<=a) (map charToInt b)))


{-grayCode levert de conversiefuncties op voor de gray codering op basis van de parameter als grondtal. De functies zijn voor flexibiliteit als helper functies geschreven, ze zijn daardoor ook buiten deze functie te gebruiken.
de functie zelf is heel simpel, plaats de 2 functies in een tupel.-}
grayCode :: Int -> ([Char] -> Int, Int -> [Char])
grayCode a = (grayCharsToInt a,grayIntToChars a)

{-De 2 conversie functies voor de gray code. grayCharsToInt converteert een cijferreeks naar een natuurlijk getal door de kijken hoeveel getallen
in de lijst graycodes niet overeenkomen met het opgegeven getal.
het volgende getal is dan de goede.
Er zou nog een check in kunnen of de parameter wel in de graycodes voor komt.
-}
grayCharsToInt :: Int -> [Char] -> Int
grayCharsToInt x y = (length(takeWhile(/=y) (gray x))) -- eventueel +1

{-
grayIntToChars geeft het n-de element uit de lijst gray codes.
-}
grayIntToChars :: Int -> Int -> [Char]
grayIntToChars x y = head (drop (y) (take (y+1) (gray x)))

{-
gray genereert de lijst met gray codes voor een gegeven grondtal.
Het algoritme dat wordt gebruikt komt van Wikipedia; spiegel de bits
en plaats een 1 voor de gespiegelde en een 0 voor de originele bits.
-}
gray :: Int -> [String]
gray 0 = [""]
gray n = let f = gray (n-1) 
         in map ('0':) f ++ map ('1':) (reverse f)

{-
De lookAndSay functie plaatst zelf de invoer voorop de lijst met uitvoer, die door de helperfuncties strLookAndSay en lasRec worden opgeleverd.
-}
lookAndSay :: Int -> [String]
lookAndSay a = show a : lasRec a

{-
Deze String Look and Say functie bouwt de afzonderlijke Strings op van de uiteindelijke lijst. Roept zichzelf hiertoe recursief aan. Wordt ook recursief aangeroepen door lasRec. Het type wordt met show geconverteerd.
-}
strLookAndSay :: [Int] -> String
strLookAndSay [] = []
strLookAndSay (a:b) = (show(l)++show(a)) ++ nextstrLookAndSay
 where l = length(takeWhile (==a) b)+1
       nextstrLookAndSay = strLookAndSay(drop (l-1) b)

{-
Look and Say Recursief; in deze functie wordt de recursie ingebouwd, en worden types heen en weer geconverteerd.
-}
lasRec :: Int -> [String]
lasRec a = f : lasRec (fromDec((map (charToInt) f)))
 where f = strLookAndSay (toDec a)

{-
De uiteindelijke Keith functie. deze roept de helper functie aan. ik heb zoveel helper functies gemaakt omdat dat flexibiliteit geeft en beter leesbaar is. De functies zijn natuurlijk gewoon te integreren.
-}
keithGetallen :: [Int]
keithGetallen = listKeithGetallen 10

{-
Met deze tussenfunctie is te bepalen vanaf welk getal naar Keith getallen wordt gezocht. Hij zoekt mbv de volgende extra functie op of het huidige getal een Keith getal is. zo ja, dan plaatst hij de uitvoer in de lijst en gaat hij verder met het volgende getal. zo nee, dan laat hij het getal weg en gaat verder.
-}
listKeithGetallen :: Int -> [Int]
listKeithGetallen 0 = []
listKeithGetallen a
 | testKeithNr a = a : listKeithGetallen (a+1)
 | otherwise = listKeithGetallen (a+1)

{-
Extra Functie om te bepalen of een getal een Keith Getal is. elem kijkt of de parameter in de Keith rij zit totdat de getallen groter worden dan de parameter zelf.
-}
testKeithNr :: Int -> Bool
testKeithNr a = elem a (takeWhile (<=a) (nextKeithNr (toDec a)))

{-
Deze helper functie genereert de reeks die bij de parameter als het begin getal hoort.
Dat gebeurt aan de hand van de definitie van de Keith-reeks; de som van de 2 laatste getallen.
-}
nextKeithNr :: [Int] -> [Int]
nextKeithNr [] = []
nextKeithNr (a:b) = a : nextKeithNr (b ++ [sum (a:b)])
