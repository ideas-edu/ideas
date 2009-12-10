{-
Tim Soethout // 3117901
GHC
-}
module Opdracht1 where
import Opdracht1Hulp

-- Opgave 1
fromDec :: [ Int ] -> Int 
fromDec = listToNumber 10

-- Opgave 2
toDec :: Int -> [Int]
toDec = numberToList 10

                            
-- Opgave 3
fromBin :: [Int] -> Int 
fromBin = listToNumber 2

-- Opgave 4
toBin :: Int -> [Int]
toBin = numberToList 2
 
                
-- Opgave 5
fromBase :: Int -> [Char] -> Int
fromBase = charListToNumber

-- Opgave 6
toBase :: Int -> Int -> [Char]
toBase = numberToCharList


-- Opgave 7
numbers :: Int -> [String] -> [(String, Int)]
numbers stelsel = map (\h -> (h, (fromBase stelsel) h))

-- Opgave 8 
{-
Helaas echt niet uitgekomen... Het idee was wel duidelijk, maar de implementatie niet.
Hieronder staan nog een aantal probeersels, mocht dat nog punten opleveren.
-}
--grayCode :: Int -> ([Char] -> Int, Int -> [Char])
--grayCode stelsel = (grayCodeCharListToInt stelsel, grayCodeIntToCharList stelsel)


--grayCodeCharListToInt :: Int -> [Char] -> Int
--grayCodeCharListToInt stelsel grayList = charListToNumber stelsel grayList

--generateGrayList :: Int -> (Int, [Char])
--generateGrayList stelsel = map (functie stelsel) [0..]
--						   		where functie stelsel getal = (getal, grey stelsel getal) 
--						   		       where
--						   		 	  		 grey stelsel getal = (mapNumberToChar stelsel getal) : (grey stelsel getal-1) 

--gray :: [Int] -> [String]
--gray stelsel  
--             | otherwise =  (map op stelsel) ++ gray (reverse stelsel)
--               where op n = [mapNumberToChar n]
{-gray :: Int -> Int -> [String]
gray _ 0 = [""]
gray stelsel n = let xs = gray stelsel (n-1) 
                     op :: [String] -> Int -> [String]
                     op t n = map ((mapNumberToChar n):) t                   
                 in map (op xs) [0..stelsel]
           -}          
--         in map ('0':) xs ++ map ('1':) (reverse xs) ++ map ('2':) xs
  
--nextGrayNumber :: Int -> [Char] -> [Char]
--nextGrayNumber stelsel number | mapCharToNumber h `rem` stelsel == stelsel-1 = '1':h:""
--                              | otherwise = [(mapNumberToChar ((mapCharToNumber h)+1))]
--                              where (h:t) = reverse number
  
-- Opgave 9
lookAndSay :: Int -> [String]
lookAndSay n = (show n) : lookAndSay (charListToNumber 10 (generateSay (numberToCharList 10 n)))           
           
-- Opgave 10 
keithGetallen :: [Int]
keithGetallen = filter op [10..]
--                where op :: Int -> Bool
op nummer = isKeith nummer (genereerKeithReeks nummer)  
isKeith n (h:t) | h > n = False
                | h == n = True 
                | h < n = isKeith n t                                                      