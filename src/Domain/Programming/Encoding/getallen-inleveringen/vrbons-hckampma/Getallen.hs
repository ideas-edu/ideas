-- hckampma : 3232093
-- vrbons   : 3220451
-- gebruikte compiler: GHC 6.10.1


module Getallen where

import Char

-- Opgave 1
-- zet een cijferreeks om in een getal
fromDec :: [Int] -> Int
fromDec [] = 0
fromDec (x:xs) = x * (10 ^ l) + fromDec xs
                 where l = length (x:xs) -1

-- Opgave 2
-- zet een getal om in een cijferreeks
toDec :: Int -> [Int]
toDec 0 = []
toDec x =  toDec(x `div` 10) ++ [mod x 10]

-- Opgave 3
-- zet een binaire cijferreeks om in een getal
fromBin :: [Int] -> Int
fromBin [] = 0
fromBin (x:xs) = x * (2 ^ l) + fromBin xs
                 where l = length (x:xs) -1

-- Opgave 4                 
-- zet een getal om naar een binaire cijferreeks                
toBin :: Int -> [Int]
toBin 0 = []
toBin x = toBin(x `div` 2) ++ [mod x 2]

-- Opgave 5
-- zet een getal uit een ander getallenstelsel om naar een getal uit het tientallig stelsel.
fromBase :: Int -> [Char] -> Int
fromBase _ [] = 0
fromBase getal (x:xs) = fromChartoInt x * (getal ^ l) + fromBase getal xs
                        where l = length (x:xs) -1
 
-- Opgave 6
-- geeft voor een getal en een grondgetal het bijbehorende getal als een lijst met Chars terug.
toBase :: Int -> Int -> [Char]
toBase _ 0 = []
toBase macht getal = toBase macht (getal `div` macht) ++ [fromInttoChar(mod getal macht)]

-- zet een Int om naar een Char (10 wordt 'a', 11 wordt 'b' enz. ) 
fromInttoChar :: Int -> Char
fromInttoChar a | a < 10 = chr (a + 48)
                | otherwise = chr (a + 87)

-- zet een Char om naar een Int (waarbij geldt dat 'a'=10 , 'b'=11 enz.)                 
fromChartoInt :: Char -> Int
fromChartoInt a | ord a < 58 = ord a - 48
                | otherwise = ord a - 87

-- Opgave 7
numbers :: Int -> [String] -> [(String, Int)]
numbers _ [] = []
numbers getal (x:xs) | elem False (map (<getal) (map fromChartoInt x)) = numbers getal xs
                     | otherwise = (x, fromBase getal x) : numbers getal xs

-- Opgave 8                     
-- levert twee functies op. één die een getal omzet in graycode of een getal in graycode omzet in een echt getal.                      
grayCode :: Int -> ([Char]->Int , Int -> [Char])
grayCode b = (fromgray,togray)
             where fromgray :: [Char] -> Int
                   fromgray l = fromBase b (map fromInttoChar(verwerk(map fromChartoInt (l))))
                                where verwerk :: [Int] -> [Int]
                                      verwerk [] = []
                                      verwerk (x:xs) = echtgetal : verwerk(veranderHead x xs)
                                                       where echtgetal = mod (x+b) b
                                                       
                   togray :: Int -> [Char]
                   togray n = map fromInttoChar (verwerk(grayLijst b n))
                              where verwerk :: [Int] -> [Int]
                                    verwerk [] = []
                                    verwerk (x:xs) = graygetal : verwerk (map ((-graygetal)+) xs)
                                                     where graygetal = mod (x + b) b

                                                     
-- bepaalt het laatste cijfer van een Int. voorbeeld: laatsteCijfer 123 geeft 3 terug.                                                     
laatsteCijfer :: Int -> Int
laatsteCijfer n = n - (n `div` 10 * 10)

-- zet een getal om in een lijst van ints en maakt gebruik van toBase om het 
-- getal om te zetten naar een ander getallenstelsel
grayLijst :: Int -> Int -> [Int]
grayLijst _ 0 = [0]
grayLijst b n = map fromChartoInt (toBase b n)

-- telt een bepaald getal op bij de head van een lijst
veranderHead :: Int ->[Int] -> [Int]
veranderHead _ [] = []
veranderHead n (x:xs) = ((n+x):xs)

-- Opgave 9
-- converteren en recursie
lookAndSay :: Int -> [String]
lookAndSay x = [a] ++ lookAndSay (read(lookAndSay' a)::Int)
               where a = show x

-- het echte rekenwerk: recursie (net zolang doorgaan totdat de lijst 
-- helemaal is doorlopen) en telt van achter naar voren hoevaak x voorkomt en plakt dat voor x.
lookAndSay' :: String -> String
lookAndSay' [] = []
lookAndSay' l = rest ++ ((show (length (takeWhile (x==) lijst))) ++ [x])
               where lijst = reverse l
                     x = head lijst
                     rest =lookAndSay'( reverse (dropWhile (x==) lijst))

                     
-- Opgave 10                     
-- Geeft de lijst van Keith getallen
keithGetallen :: [Int]
keithGetallen = filter isKeith [10..]

-- Kijkt of een bepaald getal een Keith getal is
isKeith :: Int -> Bool
isKeith l = zitIn (oneindig(toDec l)) l

-- plakt een oneindige lijst (een lijst waarvan de laatste x getallen steeds bij 
-- elkaar opgeteld zijn om het laatste element te vormen) aan de originele lijst
oneindig :: [Int] -> [Int]
oneindig l = l ++ nieuwelijst l (length l)

-- maakt een oneindige lijst, zoals bij oneindig staat omschreven
nieuwelijst :: [Int] -> Int -> [Int]
nieuwelijst l p = nieuw : nieuwelijst (l ++ [nieuw]) p
                where nieuw = optellen l p

-- telt de laatste a elementen bij elkaar op
optellen :: [Int] -> Int -> Int
optellen l a = sum (take a (reverse l))

-- kijkt of een bepaald element in een lijst zit. 
-- Werkt bij oneindige lijsten, zolang deze gerangschikt zijn
zitIn :: [Int] -> Int -> Bool
zitIn (x:xs) n | x > n = False
               | x == n = True
               | otherwise = zitIn xs n



