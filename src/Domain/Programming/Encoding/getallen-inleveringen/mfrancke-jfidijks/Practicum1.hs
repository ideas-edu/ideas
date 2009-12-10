module Practicum1 where

-- Juney Dijkstra, 3365018
-- Martine Francken, 3344827
-- Compiler: Helium, overloading uitgeschakeld

-- Opgave 1.
fromDec :: [Int] -> Int
fromDec = foldl (\n c -> 10 * n + c) 0

-- Opgave 2.
toDec :: Int -> [Int]
toDec = reverse . map(`rem` 10) . takeWhile (/= 0) . iterate (`div` 10)

-- Opgave 3.
fromBin :: [Int] -> Int
fromBin = foldl (\n c -> 2 * n + c) 0

-- Omdat we origineel waren:
fromBin' :: [Int] -> Int
fromBin' a = sum (zipWith (*) a b)
            where b = reverse . take (length a) $ (iterate (*2) 1)

-- Opgave 4.
toBin :: Int -> [Int]
toBin = reverse . map(`rem` 2) . takeWhile (/= 0) . iterate (`div` 2)

-- Opgave 5.
digitValue :: Char -> Int
digitValue c | ord c < 97 = ord c - ord '0'
             | otherwise = ord c - ord '0' - 39

fromBase :: Int -> [Char] -> Int
fromBase a d | grondgetal a && getalIsLegaal a d = foldl (\n c -> a * n + c) 0 b
             | otherwise = 0 
               where b = map digitValue d

getalIsLegaal :: Int -> [Char] -> Bool
getalIsLegaal _ [] = True
getalIsLegaal g (x : xs) | (digitValue x) > g = False
                         | otherwise = getalIsLegaal g xs

grondgetal :: Int -> Bool
grondgetal g | 2 <= g && g <= 36 = True
             | otherwise = False

-- Opgave 6.
toBase :: Int -> Int -> [Char]
toBase a b | grondgetal a = map digitChar . reverse . map(`rem` a) . takeWhile (/= 0) . iterate (`div` a) $ b
           | otherwise = []

digitChar :: Int -> Char
digitChar n | n < 10 = chr (n + ord '0')
            | otherwise = chr (n + 87)

-- Opgave 7.
numbers :: Int -> [String] -> [(String,Int)]
numbers a b = zip b c
    where c = map (fromBase a) b

-- Opgave 8.
grayCode :: Int -> ([Char] -> Int,Int -> [Char])
grayCode g = ((fromGray 0 g),(toGray 0 g))

grayLijst :: Int -> [Char]
grayLijst g = concat[charLijst, reverse charLijst]
            where charLijst = take g (map digitChar [0..35])

toGray :: Int -> Int -> Int -> [Char]
toGray _ _ 0 = "0"
toGray t g i | g^t > i = []
             | t == 0 = reverse d
             | otherwise = d
                  where d = ((cycle(concat (map (replicate (g^t)) (grayLijst g)))) !! i : (toGray (t+1) g i))

fromGray :: Int -> Int -> [Char] -> Int
fromGray t g i | eqString (toGray 0 g t) i = t
               | otherwise = fromGray (t+1) g i

-- Opgave 9.
lookAndSay :: Int -> [String]
lookAndSay x = intToString x : lijstTel x

intToString :: Int -> String
intToString a = foldr (:) "" (map (digitChar) (toDec a)) 

lijstTel :: Int -> [String]
lijstTel x = a : lijstTel (b)
        where a = map digitChar (tel 0 (head d) [] d)
                   where d = toDec x
              b = fromDec (tel 0 (head d) [] d)
                   where d = toDec x

tel :: Int -> Int -> [Int] -> [Int] -> [Int]
tel t x tx (y : ys) | x == y = tel (t+1) x tx (ys)
                    | otherwise = tel 1 y (tx ++ [t, x]) (ys)
tel t x tx [] = tx ++ [t, x]

-- Opgave 10.
keithGetallen :: [Int]
keithGetallen = keith 10

keith :: Int -> [Int]
keith x | x == last (keithLijst x) = x : keith (x + 1)
        | otherwise = keith (x + 1)

keithLijst :: Int -> [Int]
keithLijst x = takeWhile(<=x) (lijst a)
        where a = toDec x

lijst :: [Int] -> [Int]
lijst [] = []
lijst (x :xs) = b ++ lijst (xs ++ b)
        where b = [sum(x:xs)]