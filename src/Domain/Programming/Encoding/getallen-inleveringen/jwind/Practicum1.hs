-- Functioneel programmeren - Practicum 1
-- Gemaakt door Jos Wind (3345173)

module Practicum1 where

--------------------------------------
-------------- Opgave 1 --------------
--------------------------------------

fromDec :: [Int] -> Int

fromDec [] = 0
fromDec (x : xs) = x*10^length xs + fromDec xs

--------------------------------------
-------------- Opgave 2 --------------
--------------------------------------

toDec :: Int -> [Int]

toDec = reverse . map(`rem` 10) . takeWhile(/= 0) . iterate(`div` 10)

--------------------------------------
-------------- Opgave 3 --------------
--------------------------------------

fromBin :: [Int] -> Int

fromBin [] = 0
fromBin (x : xs) = x*2^length xs + fromBin xs

--------------------------------------
-------------- Opgave 4 --------------
--------------------------------------

toBin :: Int -> [Int]

toBin = reverse . map(`rem` 2) . takeWhile(/= 0) . iterate(`div` 2)

--------------------------------------
-------------- Opgave 5 --------------
--------------------------------------

-- Zet een char om in een int
charToInt :: Char -> Int
charToInt c | ord c >= ord 'a' && ord c <= ord 'z' = ord c - ord 'a' + 10
            | otherwise = ord c - ord '0'

fromBase :: Int -> [Char] -> Int
fromBase _ [] = 0
fromBase n (x : xs) = charToInt x*n^length xs + fromBase n xs

--------------------------------------
-------------- Opgave 6 --------------
--------------------------------------

-- Zet een int om in een char
intToChar :: Int -> Char
intToChar c | c >= 0 && c <= 9 = chr(ord '0' + c)
            | otherwise = chr(ord 'a' + c - 10)

toBase :: Int -> Int -> [Char]
toBase n = map intToChar . reverse . map(`rem` n) . takeWhile(/= 0) . iterate(`div` n)

--------------------------------------
-------------- Opgave 7 --------------
--------------------------------------

-- Een aangepaste versie van fromBase, die -1 geeft indien het getal niet gemaakt kan worden (fromBaseExcl 16 "faces" levert bijvoorbeeld -1 op, omdat de "s" niet voorkomt in een hexadecimaal stelsel)
fromBaseExcl :: Int -> [Char] -> Int
fromBaseExcl _ [] = 0
fromBaseExcl n (x : xs) | n < charToInt x + 1 = (-1)
                        | fromBaseExcl n xs == (-1) = (-1)
                        | otherwise = charToInt x*n^length xs + fromBaseExcl n xs

-- Maak tupels met het resultaat
numbers :: Int -> [String] -> [(String,Int)]
numbers _ [] = []
numbers n (x : xs) | fromBaseExcl n x == (-1) = numbers n xs
                   | otherwise = (x, fromBaseExcl n x) : numbers n xs

--------------------------------------
-------------- Opgave 8 --------------
--------------------------------------

-- Hier ben ik helaas niet uit gekomen.

--------------------------------------
-------------- Opgave 9 --------------
--------------------------------------

-- Maak groepjes van dezelfde getallen
toGroup :: [Int] -> [[Int]]
toGroup [] = []
toGroup (x : xs) = (x : ys) : toGroup zs
                 where (ys, zs) = toGroup' x xs
                       toGroup' i_ (i : is) | i_ == i = (i : js, ks)
                                            | otherwise = ([],[])
                                                      where (js, ks) = toGroup' i is
                       toGroup' _ is = ([], is)

-- Zet de lengte en de groepjes getallen in een lijst (dus [1,1,1] wordt bijvoorbeeld [3,1])
toList :: [Int] -> [Int]
toList = concatMap (\x -> [length x, head x]) . toGroup

-- Herhaal het maken van de lijst
list :: Int -> [[Int]]
list n = iterate toList [n]

-- Omzetten naar getallen
fromDec' :: [[Int]] -> [Int]
fromDec' [] = [0]
fromDec' (x : xs) = fromDec x : fromDec' xs

lookAndSay :: Int -> [Int]
lookAndSay = fromDec' . list

--------------------------------------
-------------- Opgave 10 --------------
--------------------------------------

-- Hier ben ik helaas niet uit gekomen.
