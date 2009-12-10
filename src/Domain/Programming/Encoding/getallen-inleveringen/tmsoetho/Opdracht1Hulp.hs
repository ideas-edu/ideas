
module Opdracht1Hulp where
import Char

--Library
--Maakt een getal van een lijst van nummers
numberToList :: Int -> Int -> [Int]
numberToList stelsel number = reverse (numberToListH stelsel number)
                            where 
                              numberToListH _ 0 = []
                              numberToListH stelsel number = (number `rem` stelsel) : (numberToListH stelsel (number `div` stelsel))
                              
-- Maakt een lijst van nummers van een getal                              
listToNumber :: Int -> [Int] -> Int
listToNumber _ [] = 0
listToNumber stelsel (h:t) = h*(stelsel^(length t)) + (listToNumber stelsel t)
								  
-- CharLists
charListToNumber :: Int -> [Char] -> Int
charListToNumber a b = listToNumber a (map mapCharToNumber b)

numberToCharList :: Int -> Int -> [Char]
numberToCharList a b = map mapNumberToChar (numberToList a b)

-- mapt getallen op characters
mapNumberToChar :: Int -> Char
mapNumberToChar n | elem n [0..15] = intToDigit n
                  | elem n [16..35] = chr (n + 87)
                  
-- mapt characters op getallen
mapCharToNumber :: Char -> Int
mapCharToNumber c | elem c ['0'..'9'] = digitToInt c
                  | elem c ['a'..'z'] = ord c - 87
                  
mapNumberToString :: Int -> String
mapNumberToString 0 = []
mapNumberToString n = intToDigit (n `mod` 10) : mapNumberToString (n `div` 10)       

-- genereert de volgende say vanuit een look
generateSay :: [Char] -> [Char]
generateSay [] = ""
generateSay list = first ++ (generateSay rest)
                    where first = fst(splitAt 2 (op list))
                          rest = snd(splitAt 2 (op list)) 
                          countht = count (head list) list                 
                          op list2 = (mapNumberToChar countht :) (drop (countht-1) list2)        
                          count _ [] = 0                                     
                          count item (h:t) | item == h = 1 + count item t    
                                           | otherwise = 0      
                                           
-- genereert de KeithReeks van een gegeven getal                                           
genereerKeithReeks :: Int -> [Int]
genereerKeithReeks getal  = charList ++ loop (length charList) charList
                       where charList = map mapCharToNumber (numberToCharList 10 getal)   
                             takeLastXNumbers x list = snd(splitAt ((length list)-x) list) 
                             loop nn l = (newNumber nn l): loop nn (l ++ [newNumber nn l]) 
                             newNumber nu li = sum (takeLastXNumbers nu li)                                                                              