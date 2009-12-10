--FP 08/09 PO1 individueel
--lwgraaff 3150968
--GHCi 6.10.1

module Main where
import Data.Char

--Opdracht 1
--vermenigvuldig de waarde met 10, en tel vervolgens het volgende decimaal erbijop
fromDec :: [Int] -> Int
fromDec = foldl ( (+).(*10) ) 0

--Opdracht 2
--voor opdracht 10 werken we met Integers
--zet het laatste decimaal achterin de lijst, en deel de rest door tien
toDec :: Integer -> [Integer]
toDec i | i < 10 = [i]
        | otherwise = toDec (i `div` 10) ++ [i `mod` 10]

-- Opdracht 3
--vergelijkbaar met opdracht 1
fromBin :: [Int] -> Int
fromBin = foldl ((+).(*2)) 0

-- Opdracht 4
--vergelijkbaar met opdracht 2
toBin :: Int -> [Int]
toBin i | i < 2 = [i]
        | otherwise = toBin (i `div` 2) ++ [i `mod` 2]

--Helper function
--leest uit char bijbehorende integer waarde af
fromChar :: Char -> Int
fromChar c | conv >= ord 'a' && conv <= ord 'z' = conv - ord 'a' + 10
           | conv >= ord '0' && conv <= ord '9' = conv - ord '0'
           | otherwise = error "Char is not an Int"
             where conv = (ord c)

--Helper function
--vormt uit int bijbehorend karakter
toChar :: Int -> Char
toChar i | i>=0 && i <=9  = chr (i + ord '0')
         | i>=9 && i <=36 = chr (i + ord 'a')
         | otherwise =  error "Int is out of range"

--Helper function
--test of chars in het bereik van de base zijn
testRange :: Int -> [Char] -> Bool
testRange i = and.map (\c -> ord c < (ord.toChar) i)

--opdracht 5
--vergelijkbaar met opdracht 1
fromBase :: Int -> [Char] -> Int
fromBase b = foldl (\i c ->i*b + (fromChar c)) 0

--opdracht 6
--vergelijkbaar met opdracht 2
toBase :: Int -> Int -> [Char]
toBase b i | i < b = [toChar i]
           | otherwise = toBase b (i `div` b) ++ [toChar (i `mod` b)]

--opdracht 7
--met testrange worden karakters buiten het bereik van de base vallen
numbers :: Int -> [String] -> [(String,Int)]
numbers b l = [(w,fromBase b w) | w <- l , testRange b w]

--opdracht 8
grayCode :: Int -> ([Char] -> Int, Int -> [Char])
grayCode b =  ((fromBase b).(grayToNat b), (\i -> natToGray b i (toBase b i)))

natToGray :: Int -> Int -> [Char] -> [Char]
natToGray b i c = natToGray' (reverse c) b i b

natToGray' :: [Char] -> Int -> Int -> Int -> [Char]
natToGray' []     _ _ _ = []
natToGray' (x:xs) b i m | even (i `div` m) = natToGray' xs b i (m*b) ++ [x]
                        | otherwise        = natToGray' xs b i (m*b) ++ [toChar ((b-1)-(fromChar x))]

grayToNat :: Int -> [Char] -> [Char]
grayToNat b c = grayToNat' c False b

grayToNat' :: [Char] -> Bool -> Int -> [Char]
grayToNat' []     _ _ = []
grayToNat' (x:xs) s b = toChar newVal : grayToNat' xs (newVal == (b-1)) b
                         where newVal | s         = (b-1 - (fromChar x))
                                      | otherwise = fromChar x

--opdracht 9
lookAndSay :: Int -> [String]
lookAndSay = look.show

look :: [Char] -> [String]
look i@(x:xs) = i : (look(count xs x 1))

count :: [Char] -> Char -> Int -> [Char]
count []     c i = show i ++ [c]
count (x:xs) c i | x == c    = count xs c (i+1)
                 | otherwise = show i ++ [c] ++ count xs x 1

--opdracht 10
--doorloop de oneindige lijst, voeg waardes toe die voldoen
keithGetallen :: [Integer]
keithGetallen = [x | x <- [10..], keith x]

--test de eerste waarde die >= i is, als deze gelijk is aan i, is het een keithgetal
keith :: Integer -> Bool
keith i = head (dropWhile (< i) (gen (toDec i))) == i

gen :: [Integer] -> [Integer]
gen l = v : (gen (drop 1 l ++ [v]))
        where v = sum l