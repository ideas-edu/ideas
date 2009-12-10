module Eerstepracticum where

-- Door: Willem Hustinx 3275116
-- Gebruikt in Helium

-- opgave 1

fromDec :: [Int] -> Int
fromDec [] = 0
fromDec (x:xs) = x * (10 ^ (length xs)) + fromDec xs

-- opgave 2

toDec :: Int -> [Int]
toDec a | a == 0    = []
        | a > 0     = toDec (div (a-m) 10) ++ [m]
        | otherwise = error "toDec: Int is kleiner als 0"
                      where m = mod a 10

-- opgave 3

fromBin :: [Int] -> Int
fromBin [] = 0
fromBin (x:xs) = x * (2 ^ (length xs)) + fromBin xs

-- opgave 4

toBin :: Int -> [Int]
toBin a | a == 0    = []
        | a > 0     = toBin (div (a-m) 2) ++ [m]
        | otherwise = error "toBin: Int is kleiner als 0"
                      where m = mod a 2

-- opgave 5

fromBase :: Int -> [Char] -> Int
fromBase _ [] = 0
fromBase a (x:xs) = charToInt x * (a ^ (length xs)) + fromBase a xs

charToInt :: Char -> Int
charToInt c | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
            | c >= 'a' && c <= 'z' = fromEnum c - fromEnum 'a' + 10
            | otherwise            = error "chatToInt: de char is niet tussen a en z of 0 en 9"

-- opgave 6

toBase :: Int -> Int -> [Char]
toBase a b | a == 0 || b == 0 = []
           | a > 0  && b > 0  = toBase a (div (b-m) a) ++ [intToChar m]
           | otherwise        = error "toBase: input moet groter als 0 zijn"
                                where m = mod b a

intToChar :: Int -> Char
intToChar i | i >= 0  && i <= 9  = toEnum (fromEnum '0' + i)
            | i >= 10 && i <= 35 = toEnum (fromEnum 'a' + i - 10)
            | otherwise          = error "intToChar: Int moet tussen de 0 en 25 zijn"

-- opgave 7

numbers :: Int -> [String] -> [(String, Int)]
numbers _ [] = []
numbers a (x:xs) | numbersCheck a x = (x, fromBase a x) : numbers a xs
                 | otherwise        = numbers a xs

numbersCheck :: Int -> String -> Bool
numbersCheck _ [] = True
numbersCheck a (x:xs) | a >= charToInt x = numbersCheck a xs
                      | otherwise        = False

-- opgave 8

grayCode :: Int -> ([Char] -> Int, Int -> [Char])
grayCode a = (fromGray, toGray)
             where fromGray :: [Char] ->Int
                   fromGray b = fromBase a (map intToChar (f(map charToInt (b))))
                                where f :: [Int] -> [Int]
                                      f []     = []
                                      f (x:xs) = (mod (x+a) a) : f(changeHead x xs)
                   toGray :: Int -> [Char]
                   toGray b = map intToChar (f (toList a b))
                              where f :: [Int] -> [Int]
                                    f []     = []
                                    f (x:xs) = d : f (map ((-d)+) xs)
                                             where d = mod (x + b) b

changeHead :: Int -> [Int] -> [Int]
changeHead _ [] = []
changeHead a (x:xs) = ((a + x) : xs)

toList :: Int -> Int -> [Int]
toList _ 0 = [0]
toList a b = map charToInt (toBase a b)

-- opgave 9

lookAndSay :: Int -> [String]
lookAndSay a = [[intToChar a]] ++ (lookAndSay2 ([intToChar a]))

lookAndSay2 :: String -> [String]
lookAndSay2 a = [b] ++ (lookAndSay2 b)
              where b = lookAndSay' 'z' 0 a

lookAndSay' :: Char -> Int -> String -> String
lookAndSay' a b c | length c <= 0 && b == 0                  = []
                  | length c <= 0 && b > 0                   = intToChar b : [a]
                  | length c >  0 && (head c) /= a && b /= 0 = intToChar b : a : lookAndSay' (head c) 1 (tail c)
                  | length c >  0 && (head c) == a           = lookAndSay' a (b+1) (tail c)
                  | otherwise                                = lookAndSay' (head c) 1 (tail c)

-- opgave 10

keithGetallen :: [Int]
keithGetallen = kg 10 

kg:: Int -> [Int]
kg a | a == (last (takeWhile (<=a) (kr a))) = [a] ++ (kg (a+1))
     | otherwise                            = (kg (a + 1))

kr :: Int -> [Int]
kr a = b ++ (kr' b (length b))
     where b = splits a

kr' :: [Int] -> Int -> [Int]
kr' a b = [c] ++ (kr' (a ++ [c]) b)
        where c = foldr (+) 0 (take b (reverse a))

splits :: Int -> [Int]
splits a | a <  10 = [a]
         | a >= 10 = (splits (div a 10)) ++ [mod a 10] 
