{-
Door: Hidde Verstoep, 3344967
Compiler: GHC 6.8.2
-}

import Char

-- Hulp functies
fromBaseIndex :: Int -> [Int] -> Int
fromBaseIndex _ [] = 0
fromBaseIndex base (i:is) = base^(length is)*i + fromBaseIndex base is

fromIndex :: [Int] -> [Char]
fromIndex = map (toLower.chr.(\x -> if x <= 9 then x+ord '0' else x+ord 'A'-10))

toBaseIndex :: Int -> Int -> [Int]
toBaseIndex _ 0 = []
toBaseIndex base x = toBaseIndex base (x `div` base) ++ [x `mod` base]

toIndex :: [Char] -> [Int]
toIndex = map ((\x -> if x<=ord '9' then x-ord '0' else x-ord 'A'+10).ord.toUpper)

-- Opdracht 1
fromDec :: [Int] -> Int
fromDec = fromBaseIndex 10

-- Opdracht 2 
toDec :: Int -> [Int]
toDec = toBaseIndex 10

-- Opdracht 3
fromBin :: [Int] -> Int
fromBin = fromBaseIndex 2

-- Opdracht 4
toBin :: Int -> [Int]
toBin = toBaseIndex 2

-- Opdracht 5
fromBase :: Int -> [Char] -> Int
fromBase base list = fromBaseIndex base (toIndex list)

-- Opdracht 6
toBase :: Int -> Int -> [Char]
toBase base x = fromIndex (toBaseIndex base x)

-- Opdracht 7
numbers :: Int -> [String] -> [(String, Int)]
numbers _ [] = []
numbers base (s:ss) = (if any (>=base) (toIndex s) then [] else [(s, fromBase base s)]) ++ numbers base ss

-- Opdracht 8
grayCode :: Int -> ([Char] -> Int, Int -> [Char])
grayCode base = (\list -> gcToInt base (toIndex list), \x -> fromIndex (gcFromInt base x))

gcToInt :: Int -> [Int] -> Int
gcToInt base list = fromBaseIndex base (gcFromInt base (fromBaseIndex base list))

gcFromInt :: Int -> Int -> [Int]
gcFromInt _ 0 = []
gcFromInt base x = let a = x `div` base
                       b = x `mod` base
                   in gcFromInt base a ++ [if a `mod` 2 == 0 then b else base-b-1]

-- Opdracht 9
lookAndSay :: Int -> [String]
lookAndSay x = show x : lasRecursive (toBaseIndex 10 x)

lasRecursive :: [Int] -> [String]
lasRecursive [] = []
lasRecursive (i:is) = let a = lasOne 1 i is
                      in a : lasRecursive (toIndex a)

lasOne :: Int -> Int -> [Int] -> String
lasOne count x [] = fromIndex [count, x]
lasOne count x (i:is) = if i==x then lasOne (count+1) i is else lasOne count x [] ++ lasOne 1 i is

-- Opdracht 10
keithGetallen :: [Int]
keithGetallen = let kgRecursive x list = (if last (kgRij x) == x then (:) x else id) (kgRecursive (x+1) list)
                in kgRecursive 10 []

-- rij wel beperken tot getallen <= aan startgetal, makkelijker controleren of het begingetal bevat.
kgRij :: Int -> [Int]
kgRij x = let a = toBaseIndex 10 x 
              kRijRecursive n list = let a = sum (take n (reverse list))
                                         b = fromBaseIndex 10 (take n list)
                                     in if a >= b then [a] else a : kRijRecursive n (list ++ [a])  
          in a ++ kRijRecursive (length a) a
