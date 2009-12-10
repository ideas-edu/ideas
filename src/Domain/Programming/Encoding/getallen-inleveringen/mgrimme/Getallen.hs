--Van Martijn Grimme, F070165, alleen uitgevoerd
--Getest met Helium
module Getallen where
import Char

--1
fromDec :: [Int] -> Int
fromDec x = fromDecRev (reverse x) 0 

fromDecRev :: [Int] -> Int -> Int
fromDecRev [] _ = 0
fromDecRev [x] y  = x * 10^y
fromDecRev (x:xs) y = x * 10^y + fromDecRev xs (y+1)

--2
toDec :: Int -> [Int]
toDec 0 = []
toDec x = toDec (div x 10) ++ [mod x 10]  

--3
fromBin :: [Int] -> Int
fromBin x = fromBinRev (reverse x) 0 

fromBinRev :: [Int] -> Int -> Int
fromBinRev [] _ = 0
fromBinRev [x] y  = x * 2^y
fromBinRev (x:xs) y = x * 2^y + fromBinRev xs (y+1)

--4
toBin :: Int -> [Int]
toBin 0 = []
toBin x = toBin (div x 2) ++ [mod x 2]


--5
fromBase :: Int -> [Char] -> Int
fromBase _ [] = 0
fromBase n x = fromBaseRev n (reverse x) 0 

fromBaseRev :: Int -> [Char] -> Int -> Int
fromBaseRev _ [] _ = 0
fromBaseRev n [x] y  = (chrToInt x) * n^y
fromBaseRev n (x:xs) y = (chrToInt x) * n^y + fromBaseRev n xs (y+1)

chrToInt :: Char -> Int 
chrToInt x     | (ord x) >= 97 && (ord x) <=122 = (ord x) - 87
               | (ord x) >= 65 && (ord x) <=90 = (ord x) - 55
               | otherwise = readInt [x]
      
--6
toBase :: Int -> Int ->[Char]
toBase _ 0 =[] 
toBase n x = toBase n (div x n) ++ [intToChr (mod x n)]        

intToChr :: Int -> Char 
intToChr x     | x >=10 && x <= 35 = chr (x +87)
               | otherwise = chr (x +48)
               
--7
numbers :: Int -> [String] -> [(String, Int)]
numbers _ [] = []
numbers n (x:xs) = (x,fromBase n x) : numbers n xs

--8
grayCode:: Int -> ( [Char] -> Int, Int -> [Char])
grayCode n = (fromGray n, toGray n)

--van getal naar Graycodering
toGray :: Int -> Int -> [Char]
toGray n i = map (intToChr) (reverse (digits n 0 i [])) 

--Zet getal om naar lijst van integer
digits :: Int -> Int -> Int -> [Int] -> [Int]
digits  n  p  i  l | n^(p+1) > i  = l++ [d]
                   | otherwise = digits n (p+1) i (l ++ [d]) 
                    where     d = digit n (p) i
                    
--Zet getal naar integer op gegeven positie                            
digit :: Int -> Int -> Int -> Int
digit n p i    | even (div based n) = mod based n      --optellen
               | otherwise = n - (mod based n) -1      --weer terugtellen
               where based = div i (n^p)
               
--van Graycodering naar getal
fromGray :: Int -> [Char] -> Int
fromGray n x =  fromBase n norm
               where norm = map (intToChr) (digitsToNorm True x n ((length x)-1))

--Zet grayCode om naar normale getalsysteem
digitsToNorm :: Bool -> [Char] -> Int -> Int -> [Int]
digitsToNorm _ [] _ _ = []
digitsToNorm norm (x:xs) n p  | norm =   int  : digitsToNorm draaivolgende xs n p 
                              | otherwise = (n-int-1) : digitsToNorm draaivolgende  xs n p 
                              where     draaivolgende = not (xor norm (even int))
                                        int= chrToInt x
--spreekt voor zich...
xor :: Bool -> Bool -> Bool
xor a b = a /= b

               

--9
lookAndSay :: Int -> [String]
lookAndSay x =  lookAndSayStr (show x)

---om heen en terug van Int naar String te voorkomen (en overload)
lookAndSayStr :: String -> [String]
lookAndSayStr x =  x : lookAndSayStr (say x)

--vertaalt een string
say :: String -> String
say x = sayStrings (splitAll [x])

--vertaalt lijst van strings bestaande uit gelijke cijfers
sayStrings :: [String] -> String
sayStrings [] = ""
sayStrings [x] = sayString x
sayStrings (x:xs)= sayString x ++ sayStrings xs

--vertaalt string bestaande uit gelijke cijfers
sayString :: String -> String 
sayString x = show(length x) ++ [head x]

--splitst strings in groepen van gelijke cijfers
splitAll :: [String] -> [String]
splitAll [] = []
splitAll [x]   |splitN x == length x = [x]        --stoppen als rest hetzelfde
               |otherwise = splitAll (split x)    --opsplitsen en opnieuw verwerken
splitAll (x:xs) = [x] ++ splitAll xs              --eerste deel koppelen met opgesplitste rest

----splitst eerste zoveel gelijke cijfers af van de rest
split ::  String -> [String]
split [] = [[]]
split x  = tupleToList(splitAt (splitN x) x)

--uitkomst van splitAt in juiste format stoppen
tupleToList :: (String, String) -> [String]
tupleToList (x,y) = [x,y]

--bepaalt positie om te splitsen
splitN :: String -> Int
splitN  []= 0
splitN  [_]= 1
splitN (x:y:xs) | x==y = 1 + splitN (y:xs)
               | otherwise = 1



--10
keithGetallen :: [Int]
keithGetallen = filter (keith) [10..]

keith :: Int -> Bool
keith i  = last (starter i) == i 

-- maakt juiste parameters om reeks te starten
starter :: Int -> [Int]
starter i = reeks i (toDec i) 
    
-- bouwt reeks op aan de hand van startwaarde, (nieuwe) lijst en aantal cijfers startwaarde
reeks :: Int -> [Int] -> [Int]
reeks i l     | i > (last l) = reeks i (volgende n l)   
              | otherwise = l   
              where n = length (toDec i)      
              
-- geeft de nieuwe lijst aan de hand van de zoveel laatsten in een lijst die opgeteld moeten worden
volgende :: Int -> [Int] -> [Int]
volgende _ []=[]
volgende n l = l ++ [sum (take n (reverse l))]