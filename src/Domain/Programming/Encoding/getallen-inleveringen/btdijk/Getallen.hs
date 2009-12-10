-- Practicum Getallen voor Functioneel Programmeren 2009
-- BT van Dijk, btdijk, 3173755
-- Opg 1 tm 7 alleen
-- Opg 8,9 en 10 in samenwerking met Taco Steemers
-- Getest met behulp van Helium 1.6

module Getallen where

import Char
-- ************************** fromDec *** OPG 1

fromDec :: [Int] -> Int
fromDec [] = 0
fromDec (x:xs) = (x*(10^length xs)) + fromDec xs

-- Deze functie moet volgens mij ook met foldr kunnen werken, het is me alleen niet gelukt
-- Als het werkend kan, zou dat vervolgens ook bij fromBin en from Base moeten kunnen

-- ************************** toDec *** OPG 2

toDec :: Int -> [Int]
toDec 0 = []
toDec x = toDec (x`div`10) ++ [x`mod`10]

-- ************************** fromBin *** OPG 3

fromBin :: [Int] -> Int
fromBin [] = 0
fromBin (x:xs) = (x*(2^length xs)) + fromBin xs

-- ************************** toBin *** OPG 4

toBin :: Int -> [Int]
toBin 0 = []
toBin x = toBin (x`div`2) ++ [x`mod`2]

-- ************************** fromBase *** OPG 5

fromBase :: Int -> [Char] -> Int
fromBase _ [] = 0
fromBase base (x:xs) = ((fromBaseHulp x)*(base^length xs)) + fromBase base xs

fromBaseHulp :: Char -> Int
fromBaseHulp n  | 48<=(ord n) && (ord n)<=57 = (ord n)-48
                | 97<=(ord n) && (ord n)<=122 = (ord n)-87
                | otherwise = error "Char out of range"

-- ************************** toBase *** OPG 6

toBase :: Int -> Int -> [Char]
toBase _ 0 = []
toBase base x = toBase base (x`div`base) ++ [toBaseHulp (x`mod`base)]

toBaseHulp :: Int -> Char
toBaseHulp n    | 0<=n && n<10 = chr(n+48)
                | 10<=n && n<36 = chr(n+87)
                | otherwise = error "Base out of range"

-- ************************** numbers *** OPG 7

numbersHulp :: Int -> [Char] -> Bool
numbersHulp _ [] = True
numbersHulp base (x:xs) = fromBase 36 [x] < base && numbersHulp base xs

numbers :: Int -> [String] -> [(String,Int)]
numbers _ [] = []
numbers base (x:xs)     | numbersHulp base x = [(x,fromBase base x)] ++ numbers base xs
                        | otherwise = numbers base xs

-- ************************** grayCode *** OPG 8
-- #### Bij deze opdracht heb ik samengewerkt met Taco Steemers

grayCode :: Int -> ([Char] -> Int, Int -> [Char])
grayCode b = (fromGrayCode b, toGrayCode b)

toGrayCode :: Int -> Int -> [Char]
toGrayCode b v = toGrayCode' b v (greatestPowerOf b v) 0

toGrayCode' :: Int -> Int -> Int -> Int -> [Char]
toGrayCode' _ _ (-1) _ = []
toGrayCode' b v p s | v<(b^p)   = ((intToChar (mod (b-s) b)) : toGrayCode' b v (p-1) (s+(mod (b-s) b)))
                    | otherwise = let a = (amountOfFits v (b^p))
                                  in  ((intToChar (mod (a+b-s) b)) : toGrayCode' b (v - ((b^p)*a)) (p-1) (s+(mod (a+b-s) b)))

fromGrayCode :: Int -> [Char] -> Int
fromGrayCode _ [] = error "error in fromGrayCode"
fromGrayCode b (c:xs) = fromGrayCode' (map (toGrayCode b) (0:[(b^(length xs))..])) ((b^(length xs))-1) (c:xs)

fromGrayCode' :: [[Char]] -> Int -> [Char] -> Int
fromGrayCode' [] _ _ = error "error in fromGrayCode'"
fromGrayCode' (x:xs) count doel | x==doel = count
                                | otherwise = fromGrayCode' xs (count+1) doel

-- Hulpfuncties

intToChar :: Int -> Char
intToChar v | v >= 0  && v <=  9   =  chr (ord '0' + v)
            | v >= 10 && v <= 35   =  chr (ord 'a' + v - 10)
            | otherwise            =  error "charToInt can only convert the numbers ranging from 0 to 35"

amountOfFits :: Int -> Int -> Int
amountOfFits x y | x==0 || y==0 = error "amountOfFits called with 0 as one or both of the arguments"
                 | otherwise    = amountOfFits' x y 0

amountOfFits' :: Int -> Int -> Int -> Int
amountOfFits' x y z | (x-y)<0   = z
                    | otherwise = amountOfFits' (x-y) y z+1

greatestPowerOf :: Int -> Int -> Int
greatestPowerOf b v = greatestPowerOf' b v 1

greatestPowerOf' :: Int -> Int -> Int -> Int
greatestPowerOf' b v p | v<(b^p)  = (p-1)
                       | otherwise = greatestPowerOf' b v (p+1)

-- ************************** lookAndSay *** OPG 9
-- #### Bij deze opdracht heb ik samengewerkt met Taco Steemers

-- Over genomen uit reader, blz 66, nog om een int naar een string om te zetten
intString :: Int -> [Char]
intString = map digitChar
                .reverse
                .map (`rem`10)
                .takeWhile(/=0)
                .iterate(`div`10)

-- Over genomen uit reader, blz 62, hulp voor intString
digitChar :: Int -> Char
digitChar n = chr (n + ord '0')

lookAndSay :: Int -> [String]
lookAndSay num = ([intString num] ++ lookAndSay (fromDec (lookAndSayHulp (0:[0]) (toDec num)))) 

lookAndSayHulp :: [Int] -> [Int] -> [Int]
lookAndSayHulp lijst [] = lijst
lookAndSayHulp [] (_ : _) = error "Error in lookAndSayHulp"
lookAndSayHulp (count:vorige) (x:xs)    | [x]==vorige = lookAndSayHulp ((count+1):vorige) xs
                                        | otherwise = (count:vorige) ++ lookAndSayHulp (1:[x]) xs

-- ************************** keithGetallen *** OPG 10
-- #### Bij deze opdracht heb ik samengewerkt met Taco Steemers

keithGetallen :: [Int]
keithGetallen = concatMap keithGetallen' [10..]

keithGetallen' :: Int -> [Int]
keithGetallen' x = keithGetallenHulp (toDec x) x

keithGetallenHulp :: [Int] -> Int -> [Int]
keithGetallenHulp [] _ = error "Error in keithGetallenHulp"
keithGetallenHulp (x:xs) doel   | (sum (x:xs))==doel = [doel]
                                | (sum (x:xs)) > doel = []
                                | otherwise = keithGetallenHulp (xs ++ [sum (x:xs)]) doel