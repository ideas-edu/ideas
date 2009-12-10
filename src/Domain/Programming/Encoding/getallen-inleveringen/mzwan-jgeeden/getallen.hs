{- De leden van ons team zijn Jim van Eeden (jgeeden, 3344800) en Marijn van der Zwan(mzwan, 3401928). 
De compiler die gebruikt is is Helium. We hebben dezelfde versie gebruikt als degene die op de practikum computers geinstalleerd staat. 
We hebben de twee functies ord en char geimporteerd omdat deze niet standaard gedefinieerd zijn.-}

module Module1 where

{- Eerste opdracht:
Hier hebben we een recursieve aanroep gebruikt. De kop van de lijst wordt vermenigvuldigt met 10^lengte staart. Vervolgens wordt 
hetzelfde uitgevoerd voor de staart. Alles wordt bij elkaar opgeteld en als de staart een lege lijst is, wordt er nog eens 0 bij de waarde opgeteld
en krijg je het decimale getal.
-}
fromDec :: [Int] -> Int
fromDec [] = 0
fromDec (x : xs) = x * 10 ^ length xs + fromDec xs

{-Tweede opdracht:
Ook hier hebben we een recursieve aanroep gebruikt. Het getal wordt door 10 gedeeld, waarna de rest vervolgens in een lijst geplaatst wordt.
Dan wordt het gehele getal gedeeld door 10, en die wordt aan de recursieve aanroep van toDec meegegeven. 
-}
toDec :: Int -> [Int]
toDec 0 = []
toDec x = toDec (x `div` 10) ++ [xs]
         where xs = x `mod` 10

{-Derde opdracht:
Deze functie werkt ongeveer hetzelfde als de eerste opdracht en dat zie je ook aan de vorm. het enige verschil is dat de input-waarde hier
binominaal is in plaats van decimaal.
-}		   
fromBin :: [Int] -> Int
fromBin [] = 0
fromBin (x : xs) = x * 2 ^ length xs + fromBin xs

{-Vierde opdracht:
Deze functie werkt hetzelfde als de tweede opdracht, maar dan voor een binominaal getallenstelsel. Eerst wordt het getal door 2 gedeeld, 
afgerondt, en vervolgens wordt de functie opnieuw toegepast op de rest van de eerste deling.
-}
toBin :: Int -> [Int]
toBin 0 = []
toBin x = toBin (x `div` 2) ++ [xs]
           where xs = x `mod` 2

{-Vijfde opdracht:
Deze functie krijgt een integer en een lijst van chars mee. De char moet omgezet worden in een integer in het getallensysteem van de eerste inputwaarde, 
de integer.De bijbehorende waarde van de char's "0" tot en met "9" zijn 48 tot en met 57. Voor een grondtal(n) tussen 2 en 9, moet de 
ord waarde van de char dus tussen 48  en 57 liggen. Als dat het geval is, wordt de ord waarde weer omgezet naar de x, en vervolgens 
vermenigvuldigt met het grondtal^lengte staart. Als het grondtal tussen de 10 en 36 ligt, wordt hetzelfde principe uitgevoerd. Vervolgens 
wordt in beide gevallen de functie recursief aangeroepen tot de staart leeg is en wordt alels bij elkaar opgeteld.
-}
fromBase :: Int -> [Char] -> Int
fromBase _ [] = 0
fromBase n (x : xs) | n >= 2 && n <= 9 && ord x >= 48 && ord x < 48 + n = (ord x - 48) * n ^ length xs + fromBase n xs
                    | n >= 10 && n <= 36 && ord x >= 97 && ord x < 97+(n-10) = (ord x - 87) * n ^ length xs + fromBase n xs
                    | otherwise = 0

{-Zesde opdracht:
Bij deze opdracht is hetzelfde principe gebruikt als in opdracht twee en vier, maar dan voor een getallenstelsel met grondtal n. 
Als de rest van het getal tussen  0 en 10 ligt wordt de rest omgezet naar een char en in een lijst gezet. Vervolgens wordt de functie opnieuw 
aangeroepen met de afgeronde waarde  van x gedeeld door n. Ditzelfde principe is gebruikt bij grondtallen groter dan 10 en kleiner gelijk aan 36.
-}
toBase :: Int -> Int -> [Char]
toBase _ 0 = []
toBase n x | xs >= 0 && xs <= 10 = toBase n (x `div` n) ++ [chr(xs + 48)]
           | xs > 10 && xs <= 36 = toBase n (x `div` n) ++ [chr(xs + 87)]
           | otherwise = []
                where xs = x `mod` n

{-Zevende opdracht:
Deze functie roept de fromBase functie aan om de string om te zetten naar een decimaal getal. Als dat getal ongelijk is aan 0 zet hij het woord plus
het overeenkomstige getal op kop van het volgende woord + getal. Als het wel gelijk is aan 0, dus als een char buiten het bereik van het getallen-
systeem valt, geeft hij een lege lijst terug.
-}
numbers :: Int -> [String] -> [(String,Int)]
numbers 0 _ = []
numbers 1 _ = []
numbers _ [] = []
numbers n (x : xs) | fromBase n x /= 0 = (x, fromBase n x) : numbers n xs
                   | otherwise = []

{-Achtste opdracht:
Deze opdracht is niet gelukt.
-}

{-Negende opdracht:
Deze functie roept een andere functie aan, de functie inhoud. Inhoud berekent de lengte tot hij een ander getal dan het begingetal tegenkomt. Wat er 
over wordt de restwaarde. Als de rest niet leeg is, wordt de lengte in een lijst gezet voor het eerste getal van de waarde. Vervolgens wordt de
functie recursief aangeroepen. Als de rest wel leeg is, gebeurd hetzelfde, maar wordt er aan het einde een lege lijst opgeteld. In lookAndSay worden
alle resultaten van inhoud bij elkaar in een lijst gezet.
-}   
lookAndSay :: Int -> [String]
lookAndSay x = showInt x : lookAndSay (inhoud x)

inhoud :: Int -> Int
inhoud x = let lengte = length (takeWhile (==(head (toDec x))) (toDec x))
               getal = head (toDec x)
               rest = dropWhile (==(head (toDec x))) (toDec x)
           in
               if rest /= [] then
               readInt((showInt lengte ++ showInt getal) ++ showInt (inhoud (fromDec rest)))
               else
               readInt((showInt lengte ++ showInt getal) ++ [])

{-Tiende opdracht:
Deze opdracht is maar deels gelukt.
 -}
keithGetal :: Int -> [Int]
keithGetal x = (toDec x)!!0 : (toDec x)!!1 : zipWith (+) (keithGetal x) (tail (keithGetal x))

{- Een aantal functies die niet in de prelude staan, maar wel nodig waren bij deze opdracht. Daarom hebben we ze geimporteerd.
-}
ord :: Char -> Int
ord = primOrd

chr :: Int -> Char
chr = primChr
