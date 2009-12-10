{- 
 Dion Gerritzen
 3220494
 GHC
-}

module Getallen where
import Data.Char --voor het gebruik van ord en chr
{-
 in opgave 1, 2, 3, 4, 8, 9 en 10 heb ik 'Int' en soms '[Int]' in de types vervangen door 'Integer',
 om grotere getallen dan die in Int passen in te kunnen voeren en op te kunnen leveren
-}

--1
fromDec :: [Int] -> Integer 
fromDec = read . map intToDigit
    --Ints in een list converteren naar Chars en het geheel converteren naar één Integer

--2
toDec :: Integer -> [Int] 
toDec = map digitToInt . show
    --een Integer converteren naar een [Char] en elke Char converteren naar een Int

toDec' :: Integral a => a -> [Integer]
toDec' = map toInteger . map digitToInt . show
    --alternatieve functie om Integers op te kunnen leveren,
    --wordt gebruikt in lookAndSay en keithGetallen

--3
fromBin :: [Integer] -> Integer
fromBin [] = 0
fromBin x@(y:ys) = y * 2^(length x - 1) + fromBin ys
    --neem het eerste getal van de list en vermenigvuldig dat met 2^(lengte van de list min 1)
    --de rest van de lijst gaat in recursie en alles wordt bij elkaar opgeteld

--4
toBin :: Integer -> [Integer]
toBin = reverse . toBin_
        where toBin_ 0 = []
              toBin_ i = i' : toBin_ (div (i - i') 2)
                         where i' = rem i 2
    --zet de rest van deling door 2 op kop van de lijst die in recursie gaat met
    --het 'overblijfsel' van het getal na deling door 2
    --als laatst wordt de lijst omgedraaid, omdat hij van hoog naar laag is opgebouwd

--5
fromBase :: Int -> [Char] -> Int
fromBase _ [] = 0
fromBase g s@(c:cs) = fromChar35 c * g^(length s - 1) + fromBase g cs
    --zelfde pricipe als bij fromBin, maar dan met een gegeven grondtal g en een conversie van Char naar Int

fromChar35 :: Char -> Int
fromChar35 c | isDigit c = ord c - 48
             | isLower c = ord c - 87
             | otherwise = error "foute invoer"
    --converteert een Char (getallen en lower case letters) naar z'n bijbehorende Int (range 0-35)

--6
toBase :: Int -> Int -> [Char]
toBase _ 0 = "0"
toBase g i = reverse (toBase_ i)
           where toBase_ 0 = []
                 toBase_ n = toChar35 n' : toBase_ (div (n - n') g)
                             where n' = rem n g
    --zelfde principe als bij toBin, maar dan met een gegeven grondtal g en een conversie van Int naar Char
    --extra case i=0, zodat de invoer 0 geen lege lijst oplevert

toChar35 :: Int -> Char
toChar35 i | i < 16 = intToDigit i
           | i > 35 = error "foute invoer"
           | i >= 16 = chr (i+87)
    --converteert een Int (range 0-35) naar z'n bijbehorende Char (getallen en lower case letters)

--7
numbers :: Int -> [String] -> [(String,Int)]
numbers _ [] = []
numbers g l = concat (map numbers_ l) --map onderstaande functie over de [String] en zorg dat alles in één lijst komt
              where numbers_ s | isValid s = [(s,fromBase g s)] --als een String 'valid' is, lever 'm op met het getal
                               | otherwise = [] --zo niet, lever een lege lijst op
                               where isValid = and . map (g >) . map fromChar35
                               --check of alle getallen in een String in het grondtal passen
                               --de functie isValid bestaat al in System.FilePath.Posix, maar ik vond de naam er goed bij passen

--8
-- grayCode :: Int -> Int -> [Char]
-- grayCode g i | i < g = toBase g i
             -- | otherwise = "hmmm"

-- grayList :: Int -> [[Char]]
-- grayList g = grayList_ g xs
             -- where grayList_ n l = grayInit ++ (grayList_ (g*n) (reverse grayInit))
                                   -- where grayInit = zipWith (++) (takegn (concat (map (replicate n) xs))) (takegn l)
                                                    -- where takegn = take (g*n)
                   -- xs = cycle (x ++ reverse x)
                   -- x = map (toBase g) [0..(g-1)]

    --deze opgave is mij niet gelukt... :(

--9
lookAndSay :: Integer -> [String]
lookAndSay i = show i : lookAndSay (read (lookAndSay_ 1 (toDec' i)))
               --zet het begingetal op kop van de lijst van de volgende lookAndSay-getallen in recursie
               where lookAndSay_ n (x:xs@(y:_)) | x == y = lookAndSay_ (n + 1) xs --als de eerste twee getallen hetzelfde zijn, kijk of er nog meer zijn
                                                | otherwise = (show n) ++ (show x) ++ lookAndSay_ 1 xs 
                                                --zet het getelde aantal getallen en het getal zelf achter elkaar en bekijk de rest van de lijst
                     lookAndSay_ n [x] = (show n) ++ (show x)
                     --bij nog maar 1 element in de lijst, zet het getelde aantal getallen en het getal zelf achter elkaar

--10
keithGetallen :: [Integer]
keithGetallen = filter isKeith [10..]
    --filter de Keithgetallen uit de (oneindige) lijst vanaf 10

isKeith :: Integer -> Bool
isKeith i | i > 9 = isKeith_ n
          | otherwise = False --getallen 0-9 zijn geen Keithgetallen
          where n = reverse (toDec' i) --omgekeerd, zodat elk volgend getal op kop van de lijst kan worden gezet
                l = length n
                isKeith_ m = if f < i then isKeith_ (f : m) --als een getal kleiner is dan het gegeven getal, zoek verder
                                      else if f == i then True --als het getal gelijkt is aan het gegeven getal, return True
                                                     else False --anders False
                             where f = (foldr (+) 0 (take l m)) --hier wordt het volgende getal uitgerekend