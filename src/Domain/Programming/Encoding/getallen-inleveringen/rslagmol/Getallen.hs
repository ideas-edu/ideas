-------------------------------------------------------------------------------
-- gegevens
-------------------------------------------------------------------------------

-- naam         :Rico Slagmolen
-- login        :rslagmol
-- studentnummer:0331805

-- compiler     :Helium 1.6

------------------------------------------------------------------------------

module Getallen where
import Char

-- Opgave 1
{-
Ik pak telkens het eerste element en dat doe ik dan maal 10 tot de n de macht
waar n de lengte van de resterende lijst is
-}

fromDec :: [Int] -> Int
fromDec [] = 0
fromDec (x:xs) = x *10^length xs + fromDec xs

-- Opgave 2
{-
Ik maak van de Int eerst een lijst van Char's (String)
met die lijst kan ik telkens het eerste element weer terug veranderen in een Int 
en dan het volgende element in de lijst, tot dat de lijst leeg is
-}
toDec :: Int -> [Int]
toDec x = intLijst (showInt x)


intLijst :: [Char] -> [Int]
intLijst [] = []
intLijst (x:xs) =  readInt [x]: intLijst xs

-- Opgave 3
{-
Hetzelfde principe als opgave 1 alleen ipv 10 tot de nde macht 2 tot de nde macht
waar n de lengte van de resterende lijst is
-}
fromBin :: [Int] -> Int
fromBin [] = 0
fromBin (x:xs) =   x * 2^length xs + fromBin xs

-- Opgave 4
toBin :: Int -> [Int]
toBin x = groterDan 2 0 x


groterDan :: Int -> Int -> Int -> [Int]
groterDan z y x | z^y > x = rekenen (z-1) z (y-1) x
                | otherwise = groterDan z (y+1) x

rekenen :: Int -> Int -> Int -> Int -> [Int]
rekenen a z 0 x | (x-a) == 0 = [a]
                | otherwise = rekenen (a-1) z 0 x
rekenen a z y x | a*z^y > x = rekenen (a-1) z y x
                | otherwise = a : rekenen (z-1) z (y-1) (x-(a*z^y))

-- Opgave 5
fromBase :: Int -> [Char] -> Int
fromBase x _ | x > 36 = error "te groot grondgetal"
fromBase _ [] = 0
fromBase x (y:ys) | lagerDan x (y:ys) = (toInt y)* x^length ys + fromBase x ys
                  | otherwise = error "buiten grondgetal"


toInt :: Char -> Int
toInt x | isDigit x = ord x - 48
        | otherwise = ord x - 87



-- ord 1-10 is -48
-- ord a-z is - 87

-- Opgave 6
toBase :: Int -> Int -> [Char]
toBase y x = toString(groterDan y 0 x)

toChar :: Int -> Char
toChar x | x < 10 = chr (x+48)
         | otherwise = chr (x+87)

toString :: [Int] -> [Char]
toString [] = ""
toString (x:xs) = toChar x : toString xs

-- Opgave 7
numbers :: Int -> [String] -> [(String,Int)]
numbers _ [] = []
numbers x (y:ys) | lagerDan x y =  (y,(fromBase x y)) : numbers x ys
                 | otherwise = numbers x ys

lagerDan :: Int -> [Char] -> Bool
lagerDan x [] = True
lagerDan x (y:ys) = toInt y <= x && lagerDan x ys

-- Opgave 8

-- Opgave 9
lookAndSay :: Int -> [String]
lookAndSay x = [(showInt x)] ++ lookAndSay (readInt (kijkLijst (showInt x)))


maalErvoor :: [Char] -> [Char]
maalErvoor (x:xs) = showInt(length(takeWhile (==x) (x:xs))) ++ (x : "")

kijkLijst :: [Char] -> [Char]
kijkLijst [] = []
kijkLijst (x:xs) = maalErvoor (x:xs) ++ kijkLijst (dropWhile (==x) (x:xs))

-- Opgave 10
keithGetallen :: [Int]
keithGetallen = tellen 10

tellen :: Int -> [Int]
tellen x | keith x (length (showInt x)) = [x] ++ tellen (x+1)
         | otherwise = tellen (x+1)

keith :: Int -> Int -> Bool
keith x n = foldl (||) False ( map (==x) (natuurGetallen (intLijst(showInt x)) x n) )

natuurGetallen :: [Int] -> Int -> Int -> [Int]
natuurGetallen x y n | last x >= y = x
                     | otherwise = natuurGetallen z y n
                                   where z = x ++ bijOptellen x n

bijOptellen :: [Int] -> Int -> [Int]
bijOptellen x n = foldl (+) 0 (lastN x n) : []


lastN :: [a] -> Int -> [a]
lastN x n = reverse (take n (reverse x))

-- sry voor het ontbreken van verdere commetaar was te laat begonnen 
