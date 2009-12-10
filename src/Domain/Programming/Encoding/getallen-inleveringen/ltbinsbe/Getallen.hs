--3132315 Ltbinsbe HELIUM
---------------------------------------------------------------------------------------------------

--Waarden
characters    :: [Char]
keithGetallen :: [Int]

characters = ['0'..'9'] ++ ['a' .. 'z']
keithGetallen = [x | x <- [10..], (inKeith x (keith (toDec x)))]
--keith berekend de keith-lijst die volgt op de lijst die als paramater is meegegeven
--inKeith controleerd of de parameter in zijn eigen keith-lijst (2e parameter) voor komt

--Opgaven
--inhoud

fromDec :: [Int] -> Int
--neemt absolute waarde en rest bij deling van 10, van iedere waarde uit de lijst

toDec :: Int -> [Int]
--neemt absolute waarde van de parameter

fromBin :: [Int] -> Int
--neemt absolute waarde en rest bij deling van 2, van iedere waarde uit de lijst

toBin :: Int -> [Int]
--neemt absolute waarde van de parameter

fromBase :: Int -> [Char] -> Int
--als een element uit de lijst geen onderdeel is van 'characters', word gedaan alsof op deze positie zich de waarde 0 bevond.

toBase :: Int -> Int -> [Char]
--neemt absolute waarde van de parameter

numbers :: Int -> [String] -> [(String, Int)]

lookAndSay :: Int -> [String]
--neemt absolute waarde van de parameter


--definities
fromDec [] = 0
fromDec xs = fromDec' 0 xs

toDec a = toDec' [] (abs a)

fromBin [] = 0
fromBin xs = fromBin' 0 (reverse xs)

toBin a = toBin' [] (abs a)

fromBase _ [] = 0
fromBase a (x:xs) | x `elem` characters = (charInt x) * (a ^ i) + fromBase a xs
                  | otherwise = 0 + fromBase a xs
                                           where i = length(x:xs) - 1

toBase b a = toBase' [] b (abs a)

numbers a xs | a < 10 = []
             | otherwise = numbers' [] xs (take (a-10) ['a' .. 'z'])

lookAndSay a = lookAndSay' (toBase 10 a)
---------------------------------------------------------------------------------------------------

--Hulpfuncties
--inhoud

fromDec'     :: Int -> [Int] -> Int
toDec'       :: [Int] -> Int -> [Int]
fromBin'     :: Int -> [Int] -> Int
toBin'       :: [Int] -> Int -> [Int]
charInt      :: Char -> Int
toBase'      :: [Char] -> Int -> Int -> [Char]

numbers'     :: [(String, Int)] -> [String] -> [Char] -> [(String, Int)]
matches      :: [Char] -> [Char] -> Bool

lookAndSay'  :: String -> [String]
lasnext      :: Int-> String -> String

keith        :: [Int] -> [Int]
inKeith      :: Int -> [Int] -> Bool

--definities

fromDec' result [] = result
fromDec' result (x:xs) =  fromDec' (result*10 + (abs x `mod` 10)) xs

toDec' result a | a < 10 = a : result
                | otherwise = toDec' ((a `mod` 10) : result) (a `div` 10)

fromBin' _ [] = 0
fromBin' a (x:xs) = 2^a * (abs x `mod` 2) + fromBin' (a+1) xs

toBin' result a | a < 2 = a : result
                | otherwise = toBin' ((a `mod` 2) : result) (a `div` 2)

charInt a = (length (takeWhile (/= a) characters))

toBase' result b a | a < b = ((!!) characters a) : result
                   | otherwise = toBase' (((!!) characters (a `mod` b)) : result) b (a `div` b)

numbers' result [] _ = result
numbers' result (x:xs) ys | matches x ys = numbers' ( result ++ (x, fromBase (10 + length ys) x) : []) xs ys
                          | otherwise = numbers' result xs ys

matches [] _ = True
matches (x:xs) ys | elem x ys = matches xs ys
                  | otherwise = False

lookAndSay' a = a : lookAndSay' (lasnext 1 a)

lasnext _ [] = []
lasnext a (x:xs) | xs == [] = (toBase 10 a ++ (x:[]))
                 | x == head xs = lasnext (a+1) xs
                 | otherwise = (toBase 10 a ++ (x:[])) ++ lasnext 1 xs

keith xs = (head xs) : keith ((tail xs) ++ (foldr (+) 0 xs):[])

inKeith _ [] = False
inKeith a (x:xs) | a > x = inKeith a xs
                 | a < x = False
                 | otherwise = True