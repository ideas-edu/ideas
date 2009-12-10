-- Door Raymond Wagenmaker
-- Gebruikte compiler: GHC 6.8.2
module Getallen where

-- Opgave 1
fromDec :: [Int] -> Int
fromDec []     = 0
fromDec (x:xs) = x * 10^length xs + fromDec xs

-- Opgave 2
toDec :: Int -> [Int]
toDec 0 = []
toDec x = toDec (x `quot` 10) ++ [x `rem` 10]

-- Opgave 3
fromBin :: [Int] -> Int
fromBin []     = 0
fromBin (x:xs) = x * 2^length xs + fromBin xs

-- Opgave 4
toBin :: Int -> [Int]
toBin 0 = []
toBin x = toBin (x `quot` 2) ++ [x `rem` 2]

-- Hulpfunctie voor opgave 5 en 7 -- zet karakters '0'...'9','a'...'z' om naar getallen 0...35
charToInt :: Char -> Int
charToInt c | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
            | c >= 'a' && c <= 'z' = fromEnum c - fromEnum 'a' + 10

-- Hulpfunctie voor opgave 6, 8 en 9 -- zet getallen 0...35 om naar karakters '0'...'9','a'...'z'
intToChar :: Int -> Char
intToChar i | i >= 0  && i <= 9  = toEnum (fromEnum '0' + i)
            | i >= 10 && i <= 35 = toEnum (fromEnum 'a' + i - 10)

-- Opgave 5
fromBase :: Int -> [Char] -> Int
fromBase g []                                   = 0
fromBase g (x:xs) | g >= 2 && g <= 36 && i <= g = i * g^length xs + fromBase g xs
                                                  where i = charToInt x

-- Opgave 6
toBase :: Int -> Int -> [Char]
toBase g 0 = []
toBase g x = toBase g (x `quot` g) ++ [intToChar (x `rem` g)]

-- Opgave 7
{- Deze functie kijkt (d.m.v. test) of alle karakters binnen een string
 - een betekenis hebben bij het grondtal en zet de bruikbare strings
 - met de bijbehorende waarde in een tupel. -}
numbers :: Int -> [String ] -> [(String, Int)]
numbers g l | g >= 2 && g <= 36 = map zetOm (filter test l)
                                  where zetOm s = (s, fromBase g s)
                                        test  s = and (map ((<=g).charToInt) s)

-- Opgave 8
{- Het idee bij het construeren van een Gray-codering is om eerst alle beschikbare
 - cijfers te gebruiken. Dan verhoog je de tweede positie van 0 (niets) naar 1 en
 - loop je in de eerste positie (meest rechts) weer terug naar 0. Dan verhoog je het
 - cijfer op de tweede positie en laat je het cijfer in positie één weer oplopen enz.-}
grayCode :: Int -> ([Char]-> Int, Int -> [Char])
grayCode g | g >= 2 && g <= 36 = (grayDecode g [], grayEncode g [])

{- Deze functie vermenigvuldigt de waarde van elk karakter met g^n (vergelijkbaar met
 - fromBase), waarin n het aantal karakters rechts van het huidige is. De karakters
 - die al verwerkt zijn worden in een tweede parameter bijgehouden. Dat is nodig om
 - de waarde te bepalen van het karakter dat verwerkt wordt. Eerst loopt positie y nl.
 - op van 0 naar g-1 en dan, na het verhogen van depositie links van y, weer af van
 - g-1 naar 0. Als alle cijfers links van x (elementen van pre) even zijn of als het
 - aantal oneven cijfers even is, dan zit x in een oplopend segment, anders in een
 - dalend. In het laatste geval is de waarde niet i, maar (g-1)-i. -}
grayDecode :: Int -> [Int] -> [Char] -> Int
grayDecode _ _   []     = 0
grayDecode g pre (x:xs) = waarde g i pre * g^length xs + grayDecode g (i:pre) xs
                          where i = charToInt x
                                waarde g x pre | foldr (==) True (map even pre) = x
                                               | otherwise                      = g - 1 -x

{- Deze functie deelt het te coderen getal door g^n, waarin n het aantal reeds gebruikte
 - posities is (nl. de lengte van het resultaat dus zover). Van het resultaat nemen we
 - de rest bij deling door 2g. We hebben dan het nummer van het element in de reeks
 - 0...g-1,g-1...0. De hulpfunctie p zorgt dat we het juiste getal uit die reeks krijgen
 - en dan zetten we het bijbehorende karakter op kop van de resultaatlijst en hiermee
 - roepen we de functie weer recursief aan om het volgende karakter te bepalen. -}
grayEncode :: Int -> [Char] -> Int -> [Char]
grayEncode g result n | q >= g       = grayEncode g (intToChar (p g (q`rem`(2*g))) : result) n
                      | q == 0       = result
                      | otherwise    = intToChar (p g (q`rem`(2*g))) : result
                                       where q = n `quot` g^length result
                                             p g x | x < g = x
                                                   | x >= g  = 2 * g - 1 - x

-- Opgave 9
{- Deze functie genereert via de hulpfunctie reeks een reeks getallen. reeks gebruikt
 - de functie next om het volgende element in de look-and-say-reeks te verkrijgen.
 - next haalt het eerste element van de lijst en telt hoe vaak datzelfde cijfer daarna
 - nog voorkomt. Het totale aantal vormt het eerste karakter van de op te leveren string
 - gevolgd door het element zelf (dus voor drie enen wordt dat "31"). Dit wordt op kop
 - van een lijst gezet die door een recursieve aanroep van next verkregen wordt. Bij
 - die aanroep wordt de lijst minus de zojuist getelde karakters meegegeven. -}
lookAndSay :: Int -> [String]
lookAndSay i = reeks [intToChar i]
               where reeks s = s : reeks (next s)
                             where next []     = []
                                   next (x:xs) = intToChar (n + 1) : x : next (drop n xs)
                                                 where n = length (takeWhile (==x) xs)

-- Opgave 10
{- Deze functie filter de Keith-getallen uit een oneindige lijst getallen groter dan 9.
 - De hulpfunctie test zet het getal om in een lijst met de cijfers van het getal en
 - geeft die lijst aan next. next berekent de som van die cijfers, voegt die som als nieuw
 - getal toe aan de lijst en laat het eerste element weg. Zo blijft next nieuwe elementen
 - berekenen tot we bij of voorbij het te onderzoeken getal zijn. In test wordt dan gekeken
 - of we precies op het getal zijn beland (dan is het dus een Keith-getal) of er voorbij. -}
keithGetallen :: [Integer]
keithGetallen = filter test [10..]
                where test n = n == next (reverse (map (`rem` 10) (takeWhile (/=0) (iterate (`quot`10) n))))
                               where next (x:xs) | som < n   = next (xs ++ [som])
                                                 | otherwise = som
                                                               where som = x + sum xs
