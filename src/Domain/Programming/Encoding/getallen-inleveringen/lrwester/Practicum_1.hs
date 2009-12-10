--
-- Functioneel Programmeren Practicum 1 (Getallen)
--
-- Lazlo Westerhof (3344975)
-- GHCi: version 6.8.2
--

--  
-- Opgave 1
-- fromDec :: [Int] -> Int
--
fromDec :: [Int] -> Int
fromDec []     = 0
fromDec (x:xs) = x * 10^length(xs) + fromDec xs

--  
-- Opgave 2
-- toDec :: Int -> [Int]
--
toDec :: Int -> [Int]
toDec 0 = []
toDec x = toDec (x `div` 10) ++ [x `mod` 10]

--  
-- Opgave 3
-- fromBin :: [Int] -> Int
--
fromBin :: [Int] -> Int
fromBin []     = 0
fromBin (x:xs) = x * 2^length(xs) + fromBin xs

--  
-- Opgave 4
-- toBin :: Int -> [Int]
--
toBin :: Int -> [Int]
toBin 0 = []
toBin x = toBin (x `div` 2) ++ [x `mod` 2]

--  
-- Opgave 5
-- fromBase :: Int -> [Char] -> Int
--
fromBase :: Int -> [Char] -> Int
fromBase _ []     = 0
fromBase n (x:xs) = i * n^length(xs) + fromBase n xs
                    where i = zoekWaarde x charToInt

-- Lijst van tupels om van Char naar de bijhorende Int te gaan 
charToInt :: [(Char,Int)]
charToInt = zip (['0' .. '9'] ++ ['a' .. 'z']) [0 .. 35]

-- Zoek tupel uit een lijst van tupels waar 'a'
-- overeenkomt met gezochte waarde en geef 'b' terug
zoekWaarde :: Eq a => a -> [(a,b)] -> b
zoekWaarde c ((a,b):ts) 
    | a == c    = b
    | otherwise = zoekWaarde c ts

--  
-- Opgave 6
-- toBase :: Int -> Int -> [Char]
--
toBase :: Int -> Int -> [Char]
toBase _ 0 = []
toBase n x = toBase n (x `div` n) ++ [c]
             where c = zoekWaarde (x `mod` n) intToChar

-- Lijst van tupels om van Int naar de bijhorende Char te gaan 
intToChar :: [(Int,Char)]
intToChar = zip [0 .. 35] (['0' .. '9'] ++ ['a' .. 'z'])

--  
-- Opgave 7
-- numbers :: Int -> [String] -> [(String,Int)]
--
numbers :: Int -> [String] -> [(String,Int)]
numbers _ []     = []
numbers n (x:xs)  
    | x == toBase n (fromBase n x) = [(x,fromBase n x)] ++ numbers n xs
    | otherwise                    = [] ++ numbers n xs
                
--  
-- Opgave 8
-- grayCode :: Int -> ([Char] -> Int,Int -> [Char])
--
-- Dit klopt niet helemaal, snap niet hoe de twee gevraagde functies nou 
-- gebruikt moeten worden. Van getal naar Gray en omgekeerd werkt 
-- wel gewoon met behulp van tussenstap naar binair stelsel.
grayCode :: Int -> ([Char] -> Int,Int -> [Char])
grayCode n = (grayToNat,natToGray)

grayToNat :: [Char] -> Int
grayToNat g = let b = grayToBin g (2^(length g))
              in fromBin (toInt b)

natToGray :: Int -> [Char]
natToGray x = let b = toChar (toBin x) 
              in binToGray b

grayToBin :: [Char] -> Int -> [Char]
grayToBin g l = if l > 1 then grayToBin (binToGray g) (l - 1)
                else g

binToGray :: [Char] -> [Char]
binToGray b = let shift = '0' : (take ((length b) - 1) b) 
              in map bitXOR (zip b shift)

-- Exclusive OR voor binaire getallen
bitXOR :: (Char,Char) -> Char
bitXOR (b1,b2) = if b1 == b2 then '0' else '1'

toInt :: [Char] -> [Int]
toInt []     = []
toInt (x:xs) = (fromEnum (x) - 48) : toInt xs

toChar :: [Int] -> [Char]
toChar []     = []
toChar (x:xs) = toEnum (x + 48) : toChar xs

--  
-- Opgave 9
-- lookAndSay :: Int -> [String]
--
lookAndSay :: Int -> [String]
lookAndSay n = map toChar (iterate say [n])

-- Maak de lookAndSay reeks van een getal
say :: [Int] -> [Int]
say []     = []
say (x:xs) = [length a + 1,x] ++ say b
             where (a,b) = span (==x) xs 

--  
-- Opgave 10
-- keithGetallen :: [Integer]
--
keithGetallen :: [Integer]
keithGetallen = [x | x <- [10 ..], isKeith x (toList x)] 

-- Kijk of een getal een Keith getal is
isKeith :: Integer -> [Integer] -> Bool
isKeith n kl
        | n == k = True
        | n < k  = False
        | n > k  = isKeith n (foldr (+) 0 (take l (kl)):kl)
        where k = head kl
              l = length(toList n)

toList :: Integer -> [Integer]
toList 0 = []
toList x = reverse(toList (x `div` 10) ++ [x `mod` 10])                     
