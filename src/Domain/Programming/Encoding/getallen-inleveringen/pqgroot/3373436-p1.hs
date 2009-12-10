-- Naam: Patrick de Groot
-- 
-- Compiler: ghc-6.10.1.-386

import Data.List
import Data.Char
import Data.String



-- opgave 1

fromDec :: [Int] -> Int
fromDec [] = 0
fromDec (x:xs) = x*(10^length xs)+fromDec xs

-- opgave 2
toDec :: Int -> [Int] 
toDec i 
	| i < 10 = [i `mod` 10]
	| otherwise =  toDec (i `div` 10) ++ [i `mod` 10]

-- opgave 3
fromBin :: [Int] -> Int
fromBin [] = 0
fromBin (x:xs) = x*(2^length xs)+fromBin xs

-- opgave 4
toBin :: Int -> [Int] 
toBin i 
	| i < 2 = [i `mod` 2]
	| otherwise =  toBin (i `div` 2) ++ [i `mod` 2]

s=['0'..'9']++['a'..'z']

-- opgave 5
fromBase :: Int -> [Char] -> Int
fromBase base [] = 0
fromBase base (x:xs) = (x `elemIndices` s)!!0*(base^length xs)+fromBase base xs

-- opgave 6
toBase :: Int -> Int -> [Char] 
toBase base i
	| i < base = [s!!(i `mod` base)]
	| otherwise = toBase base (i `div` base) ++ [s!!(i `mod` base)]


-- opgave 7

numbers :: Int -> [String] -> [(String,Int)]
numbers base [] = []
numbers base (x:xs)
	| validateString base x = [(x,(fromBase base x))]++numbers base xs
	| otherwise = numbers base xs

-- controleert de string dmv een filter van toegestane characters
validateString :: Int -> [Char] -> Bool
validateString base checkString
	| filter (`elem` fst(splitAt base s)) checkString == checkString = True
	| otherwise = False

-- opgave 8
toGrayCode :: Int -> Int -> [Char] 
toGrayCode base i
	| i < base = [s!!(i `mod` base)]
	| (i `div` base) `mod` 2 == 0 = toGrayCode base (i `div` base) ++ [s!!(i `mod` base)]
	| otherwise = toGrayCode base (i `div` base) ++ [s!!(base-(i `mod` base)-1)]

fromGrayCode :: Int -> [Char] -> Int 
fromGrayCode base [] = 0
fromGrayCode base (x:xs) 
	| (length xs) `mod` 2 == 1 = (x `elemIndices` s)!!0*(base^length xs)+fromGrayCode base xs
	| otherwise = (base-1-(x `elemIndices` s)!!0)*(base^length xs)+fromGrayCode base xs


-- opgave 9

-- 'show i' zodat er gebruik gemaakt wordt van een string ipv een integer, ivm de lengtes
lookAndSay :: Int -> [String]
lookAndSay i = lns (show i)

-- startgetal moet meegenomen worden
lns s = [s]++lns (ls (group s))

ls :: [String] -> String
ls [] = ""
ls (x:xs) = show (length(x))++[x!!0]++(ls xs)

-- opgave 10

-- er is gebruik gemaakt 
keithGetallen :: [Int]
keithGetallen = kg 10

isKeith i = iK (toDec i) (length (toDec i)) i
iK i j k
	| sum(snd(splitAt ((length i)-j) i))<k=iK (snd(splitAt 1 i)++[sum(snd(splitAt ((length i)-j) i))]) j k
	| sum(snd(splitAt ((length i)-j) i))==k = 1
	| sum(snd(splitAt ((length i)-j) i))>k = 0

kg i
	| (isKeith i)==1 = [i]++(kg (i+1))
	| otherwise = kg (i+1)

