-- Simon Veldhuijzen

-- Dit is gemaakt met behulp van de Hint interpreter van Helium. Wel is overloading disabled, aangezien er anders
-- maar weinig met chars gedaan kan worden.

module Opgaves where

-- Opgave 1

fromDec :: [Int] -> Int
fromDec [] = 0                             -- mocht een lege verzameling gegeven worden
fromDec (x:xs) = x * 10 ^ z + fromDec xs   -- bouwt het getal op uit de afzonderlijke cijfers
  where z = length(x:xs) - 1

-- Opgave 2

toDec :: Int -> [Int]
toDec x|x>0 = concat[toDec (x / 10),[x `rem` 10]]   -- splitst het getal weer op in zijn cijfers
       |otherwise = []

-- Opgave 3

fromBin :: [Int] -> Int
fromBin [] = 0
fromBin (x:xs) = x * 2 ^ z + fromBin xs    -- hetzelfde idee als opgave 1, maar dan naar een binair stelsel
  where z = length(x:xs) - 1

-- Opgave 4

toBin :: Int -> [Int]
toBin x|x>0 = concat[toBin (x / 2),[x `rem` 2]]    -- geeft het binaire getal dat hoort bij een decimaal getal
       |otherwise = []

-- Opgave 5

fromBase :: Int -> [Char] -> Int  -- y geeft hier aan hoeveeltallig het stelsel is, waarna de tekens in (x:xs)
fromBase _ [] = 0                 -- het decimale getal opbouwen dat in dit talstelsel daarmee correspondeert
fromBase y (x:xs)|(isDigit x) = (ord x - 48) * y ^ z + fromBase y xs
                 |otherwise = (ord(toUpper x) - 55) * y ^ z + fromBase y xs
  where z = length(x:xs) - 1

-- Opgave 6

toBase :: Int -> Int -> [Char]    -- Het tegenovergestelde van opgave 5: x zegt wel talstelsel gebruikt moet worden,
toBase a b|a<2 || b<1 = []        -- om daarna het decimale getal y weer te geven in dat talstelsel
toBase x y|y - x * z < 10 = concat[toBase x z,[chr(y - x * z + 48)]]
          |otherwise = concat[toBase x z,[chr(y - x * z + 55)]]
  where z = y / x

-- Opgave 7

numbers :: Int -> [String] -> [(String,Int)]   -- Kijkt welke woorden gegeven in (x:xs) overeenkomen met een 
numbers _ [] = []                              -- getal in het b-tallige talstelsel
numbers a _|a<2 = []
numbers b (x:xs)|checkChars b x = concat[[(x,fromBase b x)],numbers b xs]
                |otherwise = concat[[],numbers b xs]

checkChars :: Int -> [Char] -> Bool        -- kijkt of een bepaalde char voorkomt in een y-tallig talstelsel
checkChars _ [] = True
checkChars y (x:xs)|(isDigit x) && (ord x - 48 < y) = checkChars y xs
                   |(isAlpha x) && (ord (toUpper x) - 55 < y) = checkChars y xs
                   |otherwise = False

-- Opgave 8



-- Opgave 9

lookAndSay :: Int -> [String]
lookAndSay x|x<0 = []
           |otherwise = map lAS1 (vanaf x)

vanaf :: Int -> [Int]    -- geeft een oneindige lijst Ints, die allemaal voldoen aan het principe van look and say
vanaf n = n:vanaf(lAS2 n)

voegSamen :: [Int] -> [Int]
voegSamen [] = []                   -- plakt alle opeenvolgend dezelfde Chars aan elkaar
voegSamen (x:xs) = concat[[fromDec(takeWhile(==x) (x:xs))],voegSamen (dropWhile (==x) xs)]

telCijfers :: [Int] -> [Int]          -- telt het aantal cijfers in de Int, en zet dat getal vooraan, met daarachter
telCijfers [] = []                    -- een exemplaar van dat cijfer.   44444 wordt dus 54.
telCijfers (x:xs) = concat[[length(y)],[head y],telCijfers xs] where y = toDec x

maakChars :: [Int] -> [Char]
maakChars [] = []
maakChars (x:xs) = concat[[chr (x + 48)],maakChars xs]      -- maakt characters van de gegeven ints

lAS1 :: Int -> [Char]    -- combineert de vorige drie functies om, gegeven een int, het volgende element in de lijst
lAS1 x|x<0 = []          -- lookAndSay als Char uit te drukken
             |otherwise = maakChars (
              telCijfers (
              voegSamen (
              toDec (x))))

lAS2 :: Int -> Int       -- combineert de vorige drie functies om, gegeven een Int, het volgende element in de lijst
lAS2 x|x<0 = 0           -- vanaf als Int uit te drukken
      |otherwise = fromDec(
                   telCijfers(
                   voegSamen(
                   toDec(x))))


-- Opgave 10

keithGetallen :: [Int]
keithGetallen = filter (/=0) (tellen 14)        -- haalt de niet-keithgetallen uit de lijst

tellen :: Int -> [Int]
tellen n|vindKeith n = n:tellen(n + 1)          -- zet of een getal (als het een keith is) of een 0 in de lijst
        |otherwise = 0:tellen(n+1)

vindKeith :: Int -> Bool
vindKeith x|x<10 = False                        -- kijkt of een bepaald getal een keithgetal is
           |otherwise = kijk (lijst x) x

geefVolgende :: [Int] -> Int -> Int
geefVolgende [] _ = 0
geefVolgende (x:xs) y = foldr (+) 0 (take y (reverse(x:xs)))   -- geeft het volgende getal in een keithreeks

keithVanaf :: [Int] -> Int -> [Int]     -- bouwt een oneindige keithreeks op >> Verre van optimaal..
keithVanaf [] _ = []
keithVanaf (x:xs) y = concat[(x:xs),(keithVanaf(concat[(x:xs),[geefVolgende(x:xs) y]]) y)]

lijst :: Int -> [Int]           -- geeft van een bepaald getal de keithreeks, tot de elementen in deze reeks groter
lijst x = takeWhile (<x) (keithVanaf (toDec x) (length (toDec x)))   -- worden dan het getal zelf

kijk :: [Int] -> Int -> Bool
kijk [] _ = False
kijk (x:xs) y = y == geefVolgende (x:xs) z       -- kijkt of een getal een keithgetal is, door de lijst gegeven door
  where z = length(toDec y)                      -- lijst te nemen en de laatste z elementen bij elkaar op te tellen
                                                 -- en vervolgens te kijken of dat overeenkomt met y













