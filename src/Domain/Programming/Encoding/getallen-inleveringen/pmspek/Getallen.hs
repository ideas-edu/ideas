--Naam:			Marien Spek
--Studentnr:	3220605
--Compiler:		GHC versie 6.10.0

module Getallen where
import Data.Char
import Data.List

{-
Ik heb voor Integers gekozen ipv Int omdat de functie 'lookAndSay' al heel snel te groot is voor een Int.
Aangezien die fnctie van hulpfuncties gebruikt maakt die ook door andere functies gebruikt worden
heb ik ervoor gekozen overal Integer te gebruiken zodat het casten van Int naar Integer en andersom
minimaal blijft.
-}

--Opgave 1:
{- 
Door een foldl' te gebruiken gebruik je zo min mogelijk geheugen en is de functie tail-recursief
wat de functie erg snel maakt.
-}
fromDec :: [Integer] -> Integer
fromDec xs = foldl' (\x y -> x*10+y) 0 xs

 --Opgave 2:
{-
De functie toDec is het snelst als je het deel wat je aan het opbouwen bent steeds
weer meegeeft ipv steeds concat te gebruiken. 
-}
toDec :: Integer -> [Integer]
toDec 0 = [0]
toDec x = toDec' [] x
          where toDec' :: [Integer] -> Integer -> [Integer]
                toDec' t 0 = t
                toDec' t y = toDec' ((y `mod` 10) : t) (y `div` 10)

--Opgeve 3:
{-
Deze functie is hetzelfde als bij opdracht 1, met als verschil dat je met 2 vermenigvuldigt ipv met 10.
-}
fromBin :: [Integer] -> Integer
fromBin xs = foldl' (\x y -> x*2+y) 0 xs

--Opgave 4:
{-
Deze functie is eigenlijk hetzelfde als de functie bij opgave 2, met als verschil
dat je hier steeds _ mod 2 gebruik ipv _ mod 10 en het steeds deelt door 2 ipv 10.
-}
toBin :: Integer -> [Integer]
toBin 0 = []
toBin x = toBin' [] x
          where toBin' :: [Integer] -> Integer -> [Integer]
                toBin' t 0 = t
                toBin' t y = toBin' ((y `mod` 2) : t) (y `div` 2)

--Opgave 5:
{-
Door een foldl' te gebruiken gebruik je zo min mogelijk geheugen en is de functie tail-recursief
wat de functie erg snel maakt.
-}
fromBase :: Integer -> [Char] -> Integer
fromBase _ [] = 0
fromBase x ch = foldl' op 0 ch
                where y `op` z = y*x+ (charToInt z)

{-
Hulpfunctie die van een willekeurige Char een getal maakt,
waarbij geen onderscheid wordt gemaakt tussen hoofdletters en kleine letters.
-}
charToInt :: Char -> Integer
charToInt char | isDigit char = toInteger (ord char -48)
               | otherwise = toInteger (ord (toLower char) -87)

--Opgave 6:
{-
Om ++ niet te gebruiken heb ik de accumulerende functie toBase' geschreven die een getal uit het
10-tallig stelsel omzet naar een getal uit het x-tallig stelsel.
-}
toBase :: Integer -> Integer -> [Char]
toBase g 0 = "0"
toBase g x = toBase' [] x
             where toBase' :: [Char] -> Integer -> [Char]
                   toBase' t 0 = t
                   toBase' t yt = toBase' (intToChar (yt `mod` g) : t) (yt `div` g)

{-
Hulpfunctie die van een willekeurig getal een Char maakt.
-}
intToChar :: Integer -> Char
intToChar x | x < 10 = chr (fromInteger (48+x))
            | otherwise = chr (fromInteger (87+x))
                                      
--Opgave 7:
{-
Pas als String s binnen het x-tallig stelsel valt wordt het tupel op kop gezet van de recursieve aanroep.
-}
numbers :: Integer -> [String] -> [(String,Integer)]
numbers _ [] = []
numbers x (s:ss) | and (map ((<x).charToInt) s) = (s, fromBase x s) : numbers x ss
                 | otherwise = numbers x ss

--Opgave 8:
grayCode :: Integer -> ([Char] -> Integer, Integer -> [Char])
grayCode b = (fromGray b, toGray b)

toGray :: Integer -> Integer -> [Char]
toGray b x = toGray' 0 (toIntBase b x)
             where toGray' :: Integer -> [Integer] -> [Char]
                   toGray' _ [] = []
                   toGray' s (x:xs) = intToChar k: toGray' (s + k) xs
                                    where k = (x + b - s) `mod` b
{-
Hulpfunctie die hetzelfde doet als toBase met als enige verschil dat er niet een lijst van Char,
maar een lijst van Integers wordt opgeleverd.
-}
toIntBase :: Integer -> Integer -> [Integer]
toIntBase g 0 = [0]
toIntBase g x = toIntBase' [] x
                where toIntBase' :: [Integer] -> Integer -> [Integer]
                      toIntBase' t 0 = t
                      toIntBase' t yt = toIntBase' ((yt `mod` g) : t) (yt `div` g)

fromGray :: Integer -> [Char] -> Integer
fromGray b c = fromIntBase b (fromGray' c 0)
               where fromGray' :: [Char] -> Integer -> [Integer]
                     fromGray' [] _ = []
                     fromGray' (i:is) j = k : fromGray' is k
                                          where k = (charToInt i + j) `mod` b
{-
Hulpfunctie die hetzelfde doet als fromBase met als enige verschil dat er niet een lijst van Char,
maar een lijst van Integers als input wil hebben.
-}
fromIntBase :: Integer -> [Integer] -> Integer
fromIntBase _ [] = 0
fromIntBase x ch = foldl' op 0 ch
                   where y `op` z = y*x+ z

--Opgave 9:
{-
De functie 'split' groepeerd alle aaneengesloten gelijke characters van een lijst.
De functie 'op' maakt een lijst met daarin de lengte van de gegeven lijst en het 1e element van de gegeven lijst.
-}
lookAndSay :: Integer -> [String]
lookAndSay x = show x : lookAndSay (fromBase 10 (concatMap op (split (show x))))
               where op xs@(s:_) = intToChar (toInteger (length xs)) : [s]
                     split :: [Char] -> [[Char]]
                     split [] = []
                     split xs@(s:_) = let tuple = span (==s) xs
                                      in fst tuple : split (snd tuple)

--Opgave 10:
{-
Het eerste keithgetal is 14, daarom geef ik als 1e 14 mee om ervoor te zorgen de de functie keith,
niet nog 4x extra voor niets wordt aangeroepen aangezien x>9.
Ik heb ervoor gekozen om toDec niet te gebruiken omdat ik die lijst dan weer moeten omkeren
elke keer dat ik isKeith aan zou roepen.
-}
keithGetallen :: [Integer]
keithGetallen = keith 14
                where keith x | isKeith x (getal x)  = x : keith (x+1)
                              | otherwise = keith (x+1)
                                where getal :: Integer -> [Integer]
                                      getal 0 = []
                                      getal y = (y `mod` 10) : getal (y `div` 10)

{-
Dit is een hulpfunctie die gegeven een getal en de omgekeerde lijst die je zou krijgen bij toDec x.
Op deze manier hoef ik geen concat te gebruiken of een accumulerende functie
wat het geheel wat overzichtelijker en sneller maakt.
Wat goed uitkomt aangezien deze functie erg vaak wordt aangeroepen.
-} 
isKeith :: Integer -> [Integer] -> Bool
isKeith x xs | temp < x = isKeith x (temp : init xs)
             | temp > x = False
             | temp == x = True
               where temp = sum xs