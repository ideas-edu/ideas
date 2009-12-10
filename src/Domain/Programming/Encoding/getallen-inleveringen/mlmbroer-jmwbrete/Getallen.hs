{-\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\-
          Functioneel Programmeren eerste inleveropdracht 2008-2009
          Door Jeroen Breteler ( # 3279405 ) + Martijn Broeren ( # 3139441 )
          Compiler: GHC 6.10.1
-////////////////////////////////////////////////////////////////////////////-}


module Getallen where


-- 0. Hulpfuncties
--     Eerst twee conversiefuncties tussen Chars en Ints.
convToInt :: Char -> Int
convToInt c | value < 58 = value - 48
            | otherwise  = value - 87
            where value = fromEnum c

convToChar :: Int -> Char
convToChar i | i < 10    = toEnum (i + 48)
             | otherwise = toEnum (i + 87)

--     Shift wordt gebruikt om binnen de modulo van een
-- gegeven base verder, dan wel terug te schuiven. Deze functie wordt
-- gebruikt voor opdracht 8 over Gray codering.
shift :: Int -> Int -> Int -> Int
shift base shiftValue input = (input + boundedShiftValue + base) `mod` base
                            where boundedShiftValue = shiftValue `mod` base

-- 1. fromDec
--    Het uiteinde van de getallenlijst wordt geteld, de rest staat op z'n minst
-- één tienmacht hoger dus wordt met 10 vermenigvuldigd en in de recursie gegooid.
fromDec :: [Int] -> Int
fromDec [] = 0
fromDec getalLijst = 10 * fromDec (init getalLijst) + last getalLijst

-- 2. toDec
--    Door door 10 te delen wordt het laatste getal 'erafgedeeld',
-- dat getal wordt achteraan in de lijst gezet, en de procedure wordt herhaald.
toDec :: Int -> [Int]
toDec getal | getal `div` 10 ==  0 = getal `mod` 10 : []
            | otherwise            = toDec getalOverig ++ ((getal `mod` 10):[])
                                   where getalOverig = getal `div` 10

-- 3. fromBin
--    Identiek aan fromDec, maar 10 is door 2 vervangen..
fromBin :: [Int] -> Int
fromBin [] = 0
fromBin getalLijst = 2 * fromBin (init getalLijst) + last getalLijst

-- 4. toBin
--    Hier natuurlijk hetzelfde verhaal.
toBin :: Int -> [Int]
toBin 0 = [0]
toBin getal | getal `div` 2 ==  0 = getal `mod` 2 : []
            | otherwise            = toBin getalOverig ++ ((getal `mod` 2):[])
                                   where getalOverig :: Int
                                         getalOverig = getal `div` 2

-- 5. fromBase
--    Hier dan eindelijk de abstractie. Geen constanten meer, maar een base variabele.
-- Ook wordt er een conversiefunctie gebruikt om met chars te werken.
fromBase :: Int -> [Char] -> Int
fromBase base [] = 0
fromBase base getalLijst = base * fromBase base (init getalLijst) + convToInt (last getalLijst)

-- 6. toBase
--    Zelfde wijzigingen als bij fromBase maar dan t.o.v. toDec.
toBase :: Int -> Int -> [Char]
toBase _ 0 = ['0']
toBase base getal | getal `div` base == 0 = convToChar(getal `mod` base) : []
                  | otherwise             = toBase base getalOverig ++ (convToChar (getal `mod` base):[])
                  where getalOverig = getal `div` base

-- 7. numbers
--    De oorspronkelijk ingevoerde lijst getallen wordt gefilterd met een functie
-- die controleert of de gebruikte chars binnen de base liggen. De gefilterde lijst
-- wordt daarna gezipt met een lijst van omgezette getallen die gemaakt is met 
-- een aanroep van map met de functie fromBase.
numbers :: Int -> [String] -> [(String,Int)]
numbers _ []      = []
numbers base list = zip safeList (map (fromBase base) safeList)
                  where safeList = filter (listCheck base) list
                                 where listCheck _ []         = True
                                       listCheck base (x:xs)  = base > (convToInt x) && listCheck base xs

-- 8. grayCode
--    De uitdaging bij grayCode lag misschien meer in het begrijpen van het verschijnsel
-- van gray codering dan het daadwerkelijke programmeren. Net zoals de functie is het 
-- commentaar opgesplitst in twee delen. De hoofdfunctie roept alleen de twee delen aan.
grayCode :: Int -> ([Char] -> Int, Int -> [Char])
grayCode base = ((fromGray base), (toGray base))

-- 8.1 fromGray
--     Gray codering werkt volgens een patroon waarbij altijd geprobeerd wordt het meest naar rechts staande
-- cijfer te veranderen. De volgorde is: altijd +1, en verdergaan bij 0 als het hoogst toegestane
-- getal is bereikt en 0 nog niet is geweest. Als alle getallen op deze plek aan de beurt zijn gekomen
-- wordt er een wijziging doorgebracht in het een na rechtste cijfer. Als dat cijfer ook alle getallen binnen
-- de base is langsgegaan, gaat men weer 1 omhoog, et cetera. Het rechtste cijfer volgt daarom een bepaald patroon,
-- voor basis 3 is dat bijvoorbeeld 0 1 2 2 1 0 1 2 0, waarna het weer overnieuw begint. Dit kun je ook zien als
-- een gewone opeenvolging van 0 1 2 0 1 2 0 1 2 waarbij eens in de drie keer een getal wordt herhaald, omdat er 
-- een getal bij een cijfer dat meer naar links staat wordt gewijzigd (en vanwege gray wordt er altijd maar 1 getal
-- tegelijk gewijzigd). Met deze kennis kunnen we het patroon van 9 getallen ook onderverdelen in drie fases:
-- 0 1 2, daarna alle getallen eentje 'terug' geschoven naar 2 0 1 (ze moesten een keer 'wachten' omdat er een ander getal
-- werd gewijzigd en lopen nou eentje 'achter'), daarna nog eentje terug naar 1 2 0, en dan
-- met de derde stap terug weer bij de beginfase uitgekomen. Dit betekent dat als je weet in welke 'fase' een cijfer is
-- je ook weet wat de waarde zou zijn in een gewoon basis-3 getal. Je bepaalt de fase van het getal aan de hand van de
-- cijfers links ervan. Het meest linkse getal heeft nooit op een ander cijfer hoeven 'wachten' en staat daarom 
-- gewoon in fase 0. De waarde ervan geeft aan hoe vaak het cijfer eentje naar rechts heeft moeten wachten, dus de 
-- fase van het cijfer een naar rechts is gelijk aan de waarde van het meest linkse cijfer. Voor het derde cijfer 
-- van links geldt dat het aantal keer wachten gelijk is aan de waarde (in base-3) van het tweede cijfer van links PLUS 
-- drie keer de waarde van het meest linkse cijfer: elke keer dat dat cijfer is opgehoogd moet namelijk ook eerst 
--drie keer het tweede cijfer van links zijn opgehoogd.

fromGray :: Int -> [Char] -> Int
fromGray base grayList = fromBase base (makeNormList base grayList 0)

makeNormList :: Int -> [Char] -> Int -> [Char]
makeNormList _ [] _            = []
makeNormList base (g:gs) cycle = normChar : makeNormList base gs normValue
                               where normValue = shift base (convToInt g) cycle
                                     normChar  = convToChar normValue


toGray :: Int -> Int -> [Char]
toGray base number = makeGrayList inputList base 0
                        where inputList = toBase base number

makeGrayList [] _ _             = []
makeGrayList (x:xs) base deduct = convToChar (shift base value (0 - deduct)) : makeGrayList xs base (base*deduct + value)
                                     where value = convToInt x

-- 9. lookAndSay
--    Uit het voorbeeld in de instructie volgt dat het eerste element van de rij de integer 
-- die als parameter meegegeven wordt moet zijn ("1" is het eerste element van lookAndSay 1).
-- Daarom plakt de aangeroepen functie dit element voorop, voordat bouwLijst aangeroepen wordt,
-- die hier op verder bouwt. Deze functie blijft zichzelf oneindig lang aanroepen met zijn eigen 
-- resultaat. Dit resultaat is de functie bepaalElement. bepaalElement wordt aangeroepen met
-- een rij en kijkt eerst hoe vaak het eerste getal van die rij achter elkaar voorkomt met behulp 
-- van de functie vergelijk. Dit aantal t concateneert hij met het desbetreffende getal en vervolgens 
-- roept hij opnieuw zichzelf aan met dezelfde string, waar de eerste t elementen van gedropt zijn. 
-- Als er geen elementen meer over zijn houdt de recursie op, maar roept bouwLijst met het resultaat 
-- opnieuw bepaalElement aan.

lookAndSay :: Int -> [String]
lookAndSay a = show a: bouwLijst a

bouwLijst :: Int -> [String]
bouwLijst a = result: bouwLijst (read result::Int)
             where result = bepaalElement (show a)

bepaalElement :: [Char] -> [Char]
bepaalElement [] = ""
bepaalElement getal = t ++ [head getal] ++ bepaalElement (drop (read t::Int) getal)
                    where t = vergelijk getal 1

vergelijk :: [Char] -> Int -> [Char]
vergelijk [] _ = []
vergelijk (x:xs) k | [x] == take 1 xs = vergelijk xs (k+1)
                   | otherwise = show k

-- 10. keithGetallen
--    Omdat niet alle getallen Keithgetallen zijn, maar ze wel allemaal (boven de 9) gecheckt moeten
-- worden, is hier gebruik gemaakt van een filter. Als een cijfer een True scoort mag hij blijven, 
-- anders niet. Om te weten of een getal een Keithgetal is, moeten voor een getal van n cijfers de 
-- laatste n getallen uit de rij vergeleken worden met het begingetal. Dit doet de functie isKeith.
-- Voordat deze controle kan plaatsvinden, maakt hij eerst de lijst tot op het punt dat het nieuwste 
-- getal groter is dan het oorspronkelijke getal. Het genereren van de nieuwe getallen doet generateNext 
-- door de laatste n elementen van de huidige lijst op te tellen en deze tegen de lijst aan te plakken.
-- De functie inStukjes vormt een string om tot een lijst van integers. Dit is noodzakelijk om het 
-- begin van de lijst te maken. Hetzelfde kan gedaan worden met een aanroep van twee eerdere opgaven,
-- namelijk toDec (fromBase 10 "getal").

keithGetallen :: [Int]
keithGetallen = filter geefStukjesMee [10..]

geefStukjesMee :: Int -> Bool
geefStukjesMee a = isKeith a (inStukjes (show a))
                where inStukjes :: [Char] -> [Int]
                      inStukjes [] = []
                      inStukjes (b:bs) = convToInt b : inStukjes bs

isKeith :: Int -> [Int] -> Bool
isKeith getal lijst | last lijst < getal = isKeith getal (generateNext lijst (length (show getal)))
                    | otherwise = checkKeith getal lijst
                    where checkKeith :: Int -> [Int] -> Bool
                          checkKeith getal lijst | last lijst == getal = True
                                                 | otherwise = False

generateNext :: [Int] -> Int -> [Int]
generateNext [] _ = []
generateNext lijst aantal = lijst ++ [sum (drop (n-aantal) lijst)]
                          where n = length lijst

