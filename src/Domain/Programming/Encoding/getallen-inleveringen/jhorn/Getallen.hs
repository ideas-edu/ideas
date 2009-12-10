-------------------------------------------------------------------
-------------------------------------------------------------------
----                                                           ----
----                                                           ----
----             FUNCTIONEEL PROGRAMMEREN                      ----
----                                                           ----
----                         Practicum 1: Getallen             ----
----                                                           ----
----         Gemaakt door: Jasper Horn                         ----
----                       stud nr.: 3354903                   ----
----                       CS login: jhorn                     ----
----                                                           ----
----         Getest op: GHC 6.10.1                             ----
----                    Helium 1.6                             ----
----                                                           ----
----                                                           ----
-------------------------------------------------------------------
-------------------------------------------------------------------

import Char

fromBaseN::Int->[Int]->Int
toBaseN::Int->Int->[Int]
charToNum::Char->Int

fromDec::[Int]->Int                         -- Opgave 1
toDec::Int->[Int]                           -- Opgave 2
fromBin::[Int]->Int                         -- Opgave 3
toBin::Int->[Int]                           -- Opgave 4

fromBase::Int->String->Int                  -- Opgave 5
toBase::Int->Int->String                    -- Opgave 6

numbers::Int->[String]->[(String, Int)]     -- Opgave 7

nextLookAndSay::String->String
lookAndSay::Int->[String]                   -- Opgave 9

getKeith::Int->[Int]
isKeith::Int->Bool
keithGetallen::[Int]                        -- Opgave 10

-- Als eerste hebben we twee hulpfuncties die het daadwerkelijke brein zijn
-- achter opgave 1 t/m 7. De ene functie rekent als het een grondtal en een
-- lijst met getallen krijgt welk getal deze lijst dan voorstelt
-- De andere functie doet het tegenovergestelde, van een getal en een grondtal
-- maakt het een lijst met getallen

-- Hulpfunctie Opgave 1 t/m 7 --
fromBaseN base number = fromBaseN' base (reverse number)
    where fromBaseN' _ [] = 0
          fromBaseN' base' (c:cs) = c + base' * (fromBaseN' base' cs)

-- Hulpfunctie Opgave 1 t/m 7 --
toBaseN base number = reverse (toBaseN' base number)
    where toBaseN' base' number' | number' < base' = [number']
                                 | otherwise = number' `mod` base' : toBaseN' base' (number' `div` base')

-- Daarnaast hebben we voor Opgave 5 en 7 een functie die een character om
-- zetten in een getal. Hierbij moeten '0' t/m '9' de waardes 0 t/m 9 teruggeven
-- en 'a' t/m 'z' de waardes 10 t/m 35

-- Hulpfunctie Opgave 5 & 7 --
charToNum c | c >= '0' && c <= '9' = ord c - ord '0'
            | c >= 'a' && c <= 'z' = ord c - ord 'a' + 10
            | isUpper c = charToNum (toLower c)
            | otherwise = error "charToNum: char moet alphanumeriek zijn"

-- Met de hulpfuncties fromBaseN en toBaseN stellen opgave 1 t/m 4 
-- niets meer voor

-- Opgave 1 --
fromDec = fromBaseN 10

-- Opgave 2 --
toDec = toBaseN 10

-- Opgave 3 --
fromBin = fromBaseN 2

-- Opgave 4 --
toBin = toBaseN 2

-- Bij opgave 5 moeten we charToNum nog even mappen, maar echt moeilijk
-- is het niet

-- Opgave 5 --
fromBase base string | base > 36 = error "fromBase: Maximum grondtal is 36"
                     | base < 2  = error "fromBase: Minimum grondtal is 2"
                     | otherwise = fromBaseN base (map charToNum string)

-- Hier hebben we het tegenovergestelde van charToNum nodig,
-- maar omdat dat niet meer is dan (alphanumeriek !!) met een
-- lijstdefinitie voor alphanumeriek in de where en omdat deze functie
-- verder nergens wordt gebruikt (iig niet de complete functie)
-- heb ik hem in deze functie gehouden

-- Opgave 6 --
toBase base number | base > 36 = error "toBase: Maximum grondtal is 36"
                   | base < 2  = error "toBase: Minimum grondtal is 2"
                   | otherwise = map (alphanumeriek !!)(toBaseN base number)
    where alphanumeriek = ['0' .. '9'] ++ ['a' .. 'z']

-- Bij opgave 7 beginnen we met het eruit filteren van de woorden die niet mogen
-- Dit doen we door te filteren op de woorden die "Als je de letters omzet en
-- en dan voor elke letter kijkt of deze lager is dan het grondtal en alleen
-- woorden toe te staan waarbij dit bij alle letters zo is"
-- Vervolgens gooien we het resultaat in numbers', waarin we simpelweg fromBase
-- gebruiken om een van elk woord in de lijst ook de waarde te berekenen

-- Opgave 7 --
numbers n lijst = numbers' n (filter (and.(map ((< n).charToNum))) lijst)
    where numbers' _  [] = []
          numbers' n' (c:cs) = (c, fromBase n' c) : numbers n' cs

-- Hier hebben we een functie die als je hem een getal in de vorm van een
-- string meegeeft, het lookAndSay getal vindt dat er na komt
-- We kijken in feite wat het eerste getal is dat we tegenkomen, en dan
-- hoe vaak het er nog na komt, dit is 1 lager dan het aantal dat er dus
-- achter elkaar staat. Dit wordt opgelost doordat hier numeric bij '1' begint

-- Hulpfunctie Opgave 9 --
nextLookAndSay [] = ""
nextLookAndSay (c:cs) = numeric !! (length (takeWhile (== c) cs)): c : nextLookAndSay (dropWhile (== c) cs)
    where numeric = ['1' .. '9']

-- Vervolgens hoeven we alleen de eerste keer het getal om te zetten in een
-- string, en elke keer nextLookAndSay uit te voeren

-- Opgave 9 --
lookAndSay n = lookAndSay' (toString n)
    where toString n' = reverse (toString' n')
          toString' n'' | n'' < 10  = numeric !! n'' : []
                        | n'' >= 10 = numeric !! (n'' `div` 10) : toString' n''
          numeric = ['0' .. '9']
          lookAndSay' string = string : lookAndSay' (nextLookAndSay string)

-- We hebben hier een functie die een complete Keith reeks voor een bepaald
-- getal geeft. Om het zo efficient mogelijk te doen vormen we de output gewoon
-- door steeds een element op kop te zetten, maar ondertussen geven we ook de
-- eerste n (= lengte van orignele getal) getallen in een lijst mee naar de
-- volgende aanroep. Deze getallen staan in omgekeerde volgorde zodat we
-- de cons kunnen gebruiken.

-- Hulpfunctie Opgave 10 --
getKeith n = splitDigits n ++ getKeith' (length (splitDigits n)) (reverse (splitDigits n))
    where getKeith' n' list = nextKeith : getKeith' n' (nextKeith : (take (n' - 1) list))
              where nextKeith = sum list
          splitDigits n' = reverse (splitDigits' n')
          splitDigits' n'' | n'' < 10  = [n'']
                           | n'' >= 10 = n'' `mod` 10 : splitDigits' (n'' `div` 10)

-- isKeith kijkt of een getal een Keithgetal is. het neemt simpelweg het
-- eerste getal van de lijst dat niet lager is dan het getal, en kijkt of
-- dat gelijk is aan het getal

-- Hulpfunctie Opgave 10 --
isKeith n = head (dropWhile (< n) (getKeith n)) == n

-- keithGetallen is met die twee hulpfuncties niet meer moeilijk, nu hoeven
-- we alleen nog maar alle getallen boven de negen te filteren op de waarde die
-- isKeith geeft.

-- Opgave 10 --
keithGetallen = filter isKeith [10..]