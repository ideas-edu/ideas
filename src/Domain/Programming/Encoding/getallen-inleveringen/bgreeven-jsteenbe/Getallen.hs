module Getallen where
import Char

{-
Bart van Greevenbroek - studentnummer 3117812
Jolanda van Steenbergen - studentnummer 3241106
Wij hebben compiler GHC versie 6.8.2 gebruikt.
-}

-- opdracht 1
-- deze functie produceert een cijfer uit een lijst van getallen,
-- en dit is de som van machten van 10, dus dit kan geschreven 
-- worden als een foldl: 

fromdec :: [Int] -> Int
fromdec = foldl (\a b -> 10*a+b) 0

-- opdracht 2
-- deze functie gaat de andere kant op. het maakt gebruik van div 
-- en mod om recursief een lijst op te bouwen.

toDec :: Int -> [Int]
toDec 0 = []
toDec x = toDec (x `div` 10) ++ [x `mod` 10]

-- opdracht 3
-- deze functie zet een binaire string om in het getal dat het representeerd.

fromBin :: [Int] -> Int
fromBin [] = 0
fromBin (x:xs) = x * 2^(length xs) + fromBin xs

-- opdracht 4
-- toBin zet een getal om in een binaire string door de modulo van twee (1 of 0) op kop te zetten van toBin (div x 2) 

toBin :: Int -> [Int]
toBin x | x == 0 = []
        | otherwise = (mod x 2) : toBin (div x 2)

-- opdracht 5
-- fromBase is eigenlijk fromBin maar nu is het grondtal variabel.
-- van fromBase hebben we een Maybe Int gemaakt omdat sommige characters
-- niet ingevoerd mogen worden, en dit wordt ook gecheckt in opdracht 7.
 
fromBase :: Int -> [Char] -> Maybe Int
fromBase _ [] = Just 0
fromBase n (x:xs) | ord x > ord 'a' + n-11 = Nothing
                  | otherwise = case y of
                                   Nothing -> Nothing
                                   Just xr -> Just (fromChartoInt x * n^(length xs) + xr)
                      where y = fromBase n xs
                         
-- deze functie maakt van een character het benodigde getal. voor deze functie moesten
-- wij import Char toevoegen voor isLower en isDigit.
fromChartoInt :: Char -> Int
fromChartoInt c | isLower c       = ord c - ord 'a' +10
                | isDigit c       = ord c - ord '0'
                | otherwise = error "geen cijfer of getal."

-- opdracht 6
-- deze functie past reverse toe op het resultaat van toBase'. 
toBase :: Int -> Int -> [Char]
toBase n g = reverse (toBase' n g)

-- bij toBase' wordt het echte werk gedaan.
toBase' :: Int -> Int -> [Char]
toBase' n g | g < n     = [fromInttoChar g]
            | otherwise = fromInttoChar(mod g n) : toBase' n (div g n)

-- deze functie zet een int om in het benodigde character.
fromInttoChar :: Int -> Char
fromInttoChar n | n >= 0 && n <= 9 = chr (n+ ord '0')
                | otherwise = chr (n + ord 'a' -10)

-- opdracht 7

numbers :: Int -> [String] -> [(String,Int)]
numbers _ [] = []
numbers n (x:xs) = let result = fromBase n x
                   in case result of
                      Nothing -> numbers n xs
                      Just xr -> (x,xr) : numbers n xs

-- opdracht 8

--graycode :: Int -> ([Char] -> Int, Int -> [Char])
-- niet meer aan toe gekomen.

-- opdracht 9

lookAndSay :: Int -> [String]
lookAndSay n = next [[fromInttoChar n]]

-- next voegt steeds het volgende element (een String) aan de lijst toe
-- dat element wordt berekend met behulp van de hulpfunctie nieuw
next :: [String] -> [String]
next begin = begin ++ next (nieuw (last begin) "" 1)

-- nieuw krijgt als eerste argument de laatste string uit de look-and-say-lijst mee,
-- als tweede argument een aanvankelijk lege string om daarin het volgende element op te bouwen,
-- en als derde argument een accumulator om steeds te tellen hoeveel keer een character achter elkaar voor komt.
nieuw :: String -> String -> Int -> [String]
nieuw [] _ _ = []
nieuw (c:cs) el acc | cs == ""     = [(el ++ [fromInttoChar acc] ++ [c])]
                    | c == head cs = nieuw cs el (acc+1)
                    | otherwise    = nieuw cs (el ++ ([fromInttoChar acc] ++ [c])) 1

-- opdracht 10

-- de keithgetallen beginnen bij getallen groter dan 9, dus filteren wij
-- isKeith uit een oneindige lijst.

keithGetallen :: [Int]
keithGetallen = filter isKeith [10..]

-- isKeith checkt of iets een keith getal is. Dit wordt gedaan door de hulpfunctie iskeith'
isKeith :: Int -> Bool
isKeith x = isKeith' toDecx (length toDecx) x
     where toDecx = toDec x

{- isKeith' krijgt als eerste argument een toDec getal binnen, bijvoorbeeld [1,4],
daarna de lengte van deze lijst, en vervolgens het getal zelf. Met patternguards
wordt er gechecked of de gegenereerde rij gelijk is of voorbij het getal dat de 
rij heeft gegenereerd. Respectievelijk hoort het wel of niet tot de Keith getallen. 
In het andere geval genereerd het het volgende getal in de rij, totdat isKeith' 
matched op een van de twee bovenste gevallen. -}

isKeith' :: [Int] -> Int -> Int -> Bool
isKeith' list n getal | lastlist > getal  = False
                      | lastlist == getal = True
                      | otherwise         = isKeith' (list ++ [sum (drop ((length list)-n) list)]) n getal
                           where lastlist = last list