{- 
Functioneel programmeren
Practicum 1: Getallen

Robert Hensing, 3361063
Gabe Dijkstra, 3354881

Gebruikte compiler: GHC 6.8.2
-} 

import Char

-- Opgave 1

-- Maak van een lijst van [Int] cijfers een Int.
fromBaseI :: Int -> [Int] -> Int
fromBaseI base = foldl op 0
    where op a b | abs b < base = (abs a * base + abs b) * sign a * sign b
                 | otherwise = error "Invoer cijfers groter dan basis."
              where sign x | x < 0 = -1
                           | otherwise = 1

-- Maak van een lijst van [Int] decimale cijfers een Int
-- Speciaal geval van fromBaseI
fromDec :: [Int] -> Int
fromDec = fromBaseI 10

-- Opgave 2
-- Hulpfunctie voor toBaseI
-- Maak een little endian lijst van cijfers uit een Int
-- Geeft echter de lege lijst voor 0
toBaseI' :: Int -> Int -> [Int]
toBaseI' _ 0 = []
toBaseI' base x = r : toBaseI' base q
                  where (q, r) = divMod x base

-- Maak een lijst van cijfers uit een Int
toBaseI :: Int -> Int -> [Int]
toBaseI base 0 = [0]
toBaseI base n | n > 0 = reverse (toBaseI' base n)
               | n < 0 = (0 - head l) : tail l
                        where l = reverse (toBaseI' base (0-n))

toDec :: Int -> [Int]
toDec = toBaseI 10                      

-- Opgave 3
fromBin :: [Int] -> Int
fromBin = fromBaseI 2

-- Opgave 4
toBin :: Int -> [Int]
toBin = toBaseI 2

-- Opgave 5
-- 
charToInt :: Char -> Int
charToInt c | c >= '0' && c <= '9' = (Char.ord c) - (Char.ord '0')
            | c >= 'a' && c <= 'z' = (Char.ord c) - (Char.ord 'a') + 10
            | c >= 'A' && c <= 'Z' = (Char.ord c) - (Char.ord 'A') + 10

fromBase :: Int -> [Char] -> Int
fromBase base str = fromBaseI base (map charToInt str)

-- Opgave 6
intToChar :: Int -> Char
intToChar n | n >=  0 && n <=  9 = Char.chr (n      + Char.ord '0')
            | n >= 10 && n <= 36 = Char.chr (n - 10 + Char.ord 'a')

toBase :: Int -> Int -> [Char]
toBase base n = map intToChar (toBaseI base n)

-- Opgave 7

isValidDigit :: Int -> Char -> Bool
isValidDigit base c | base >=  0 && base <= 10 = c >= '0' && c < Char.chr ((Char.ord '0') + base)
                    | base > 10 && base <= 36 = isValidDigit 10 c || (
                                                toLower c >= 'a' &&
                                                toLower c < Char.chr ((Char.ord 'a') + base - 10)
                                                )

numbers :: Int -> [String] -> [(String, Int)]
numbers base words = map (\word -> (word, fromBase base word)) (filter valid words)
                     where valid word = and (map (isValidDigit base) word)

-- Opgave 8

fromGrayCode :: Int -> [Int] -> [Int]
fromGrayCode base grayDigits = reverse digits where
    (digits, _) = foldl step ([], 0) grayDigits
        where
          step :: ([Int], Int) -> Int -> ([Int], Int)
          step (digits, shift) gd = (digit : digits, shift + digit)
              where
                digit = (gd + shift) `mod` base

toGrayCode :: Int -> [Int] -> [Int]
toGrayCode base digits = reverse gc where
    (gc, _) = foldl step ([], 0) digits
        where 
          step :: ([Int], Int) -> Int -> ([Int], Int)
          step (gds, shift) digit = (gd : gds, shift + gd)
              where
                gd = (digit + base - shift) `mod` base

grayCode :: Int -> ([Char] -> Int, Int -> [Char])
grayCode base = (fromBaseI base . fromGrayCode base . map charToInt,
                map intToChar . toGrayCode base . toBaseI base)
                
-- Opgave 9

lookAndSay' :: String -> String
lookAndSay' numStr = naarTekst (foldr acc [] (map charToInt numStr))
             where
                acc :: Int -> [(Int, Int)] -> [(Int, Int)]
                acc i [] = [(i, 1)]
                acc i ((current, count) : previous)
                     | current == i = (current, count + 1) : previous
                     | otherwise = (i, 1) : (current, count) : previous
                naarTekst [] = []
                naarTekst ((val, count) : pairs) = (toBase 10 count) ++ (toBase 10 val) ++ naarTekst pairs

lookAndSay :: Int -> [String]
lookAndSay i = toBase 10 i : makeList (toBase 10 i)
           where makeList str = result : makeList result
                                where result = lookAndSay' str


-- Opgave 10

keithGetallen :: [Integer]
keithGetallen = filter isKeithGetal [10..]

isKeithGetal :: Integer -> Bool
isKeithGetal n = contains n (keithReeks n)
    where
      contains :: Integer -> [Integer] -> Bool
      contains key list | head list == key = True
                        | head list > key = False
                        | otherwise       = contains key (tail list)

keithReeks :: Integer -> [Integer]
keithReeks n = begin ++ genereerReeks begin
    where begin = map toInteger (toDec (fromInteger n))
          genereerReeks :: [Integer] -> [Integer]
          genereerReeks list = nieuw : (genereerReeks ((tail list) ++ [nieuw]))
              where nieuw = sum list
