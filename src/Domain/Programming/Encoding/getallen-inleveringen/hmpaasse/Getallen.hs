-- Hiram van Paassen 3034364
-- Compiler: GHC 6.10.1

import List
import Char

-- modified versions of digitToInt and intToDigit because they only go till 'f' or 15
digitToIntZ :: Char -> Int
digitToIntZ c
  | isDigit c            =  fromEnum c - fromEnum '0'
  | c >= 'a' && c <= 'z' =  fromEnum c - fromEnum 'a' + 10
  | c >= 'A' && c <= 'Z' =  fromEnum c - fromEnum 'A' + 10
  | otherwise            =  error "digitToInt: not a digit"

intToDigitZ :: Int -> Char
intToDigitZ i
  | i >= 0  && i <=  9   =  toEnum (fromEnum '0' + i)
  | i >= 10 && i <= 36   =  toEnum (fromEnum 'a' + i - 10)
  | otherwise            =  error "intToDigit: not a digit"


-- hulp functies
-- fromSys kan in 1 regel maar dan zonder error, maar het is wel netjes om die te gooien omdat je ander onzin krijgt
fromSys :: Int -> [Int] -> Int
fromSys s l = foldl (+) 0 (snd (mapAccumR convertNthDigit 0 l))
            where convertNthDigit x y | y < s     = (x+1, y*s^x)
                                      | otherwise = error "number is greater then base"

toSys :: Int -> Int -> [Int]
toSys _ 0 = [0]
toSys s i = reverse(toSys' s i)
            where toSys' _ 0 = [] 
                  toSys' x y = (y`rem`x: toSys' x (y`div`x))


--opdracht 1
fromDec :: [Int] -> Int 
fromDec = fromSys 10

--opdracht 2
toDec :: Int -> [Int] 
toDec = toSys 10

--opdracht 3
fromBin :: [Int] -> Int 
fromBin = fromSys 2

--opdracht 4
toBin :: Int -> [Int] 
toBin = toSys 2

--opdracht 5
fromBase :: Int -> [Char] -> Int 
fromBase b l = fromSys b (map digitToIntZ l)

--opdracht 6
toBase :: Int -> Int -> [Char] 
toBase b i= map intToDigitZ (toSys b i)

--opdracht 7
numbers :: Int -> [String] -> [(String, Int)] 
numbers i l = zip l (map (fromBase i) l)


--opdracht 8
grayCode :: Int -> ([Char ] -> Int, Int -> [Char])
grayCode b = (grayFrom b, grayTo b)
  
grayFrom :: Int -> [Char] -> Int                   
grayFrom b l = fromSys b (snd (mapAccumL op 0 (map digitToIntZ l)))
                                   where op x y = (org, org)
                                                  where org = mod (x + y) b
                    
grayTo :: Int -> Int -> [Char]              
grayTo b i = map intToDigitZ (snd (mapAccumL op 0 (toSys b i)))
                                 where op x y = (x + gray, gray)
                                                where gray = mod (y+b-x) b                  
              
--opdracht 9
lookAndSay :: Int -> [String]
lookAndSay i = ((show i):lookAndSay l)
               where l =  (fromDec.concat) (zipWith (\x y -> [x,y]) (map length (group sortedList)) (nub sortedList) )
                       where sortedList = (sort.toDec) i


--opdracht 10
keithGetallen :: [Int]
keithGetallen = [x | x <- [1..], x > 9 ,keith x]
                       where keith x = ((elem x).(takeWhile (<=x)).(dropWhile (<x))) (keithList x)

keithList :: Int -> [Int]                     
keithList x = let list = numberAsList ++ (taillist ((length numberAsList)-1))
                    where taillist 0 = list
                          taillist i = zipWith (+) list (tail (taillist (i-1)))
                          numberAsList = toDec x
              in list
                             