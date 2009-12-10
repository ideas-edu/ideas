-- Practicum Functioneel Programmeren
-- door: Arthur Nieuwland(3373444) en Jordy van Leersum(3344878)

-- Declaraties
digits = ['0'..'9']++['a'..'z']

-- Opdracht 1
fromDec :: [Int] -> Int 
fromDec []     = 0
fromDec (x:xs) = x*b + fromDec xs
               where b = 10^length xs

-- Opdracht 2
toDec :: Int -> [Int] 
toDec 0 = []
toDec x = toDec ((x-b) `div` 10) ++ [b]
        where b = x `mod` 10

-- Opdracht 3
fromBin :: [Int] -> Int
fromBin []     = 0
fromBin (x:xs) = x*b + fromBin xs
               where b = 2^length xs

-- Opdracht 4
toBin :: Int -> [Int]
toBin 0 = []
toBin x = toBin (x `div` 2) ++ [b]
        where b = (x `mod` 2)

-- Opdracht 5
fromBase :: Int -> String -> Int
fromBase b s | validStr b s = fromBase' b s
             | otherwise    = -1 -- Errorcode

fromBase' :: Int -> String -> Int
fromBase' b []     = 0
fromBase' b (c:cs) = i*d + fromBase' b cs
                   where d = b^length cs
                         i = charToInt c

validStr :: Int -> [Char] -> Bool -- Behoren alle chars tot de base
validStr i s = and(map (\x -> elem x (take i digits)) s)
					
charToInt :: Char -> Int
charToInt c = charToInt' c b 0
            where b = digits

charToInt' :: Char -> [Char] -> Int -> Int
charToInt' ch (c:cs) i | ch == c   = i
                       | otherwise = charToInt' ch cs (i+1)
charToInt' _ "" _ = 0 -- Wanneer je char zoekt dat niet bestaat is antwoord 0.

-- Opdracht 6
toBase :: Int -> Int -> [Char]
toBase b i = toBase' b i ""

toBase' :: Int -> Int -> [Char] -> [Char]
toBase' _ 0 [] = "0" -- Retourneer 0 als je 0 vertaalt
toBase' _ 0 s  = s   -- Plak geen onnodige 0 vooraan.
toBase' b i s  = toBase' b (i `div` b) (toDigit(i `mod` b) : s)

toDigit :: Int -> Char
toDigit i = head(drop i digits)

-- Opdracht 7
numbers :: Int -> [String] -> [(String, Int)]
numbers i s = filter (\x -> snd x >= 0) $ map (fromBaseTuple i) s

fromBaseTuple :: Int -> String -> (String, Int)
fromBaseTuple i s = (s, fromBase i s)

-- Opdracht 8



-- Opdracht 9
lookAndSay :: Int -> [String]
lookAndSay i = toBase 10 i : lookAndSay' i

lookAndSay' :: Int -> [[Char]]
lookAndSay' i = [x] ++ lookAndSay' (fromBase 10 (x))
              where x = lookAndSay'' (toBase 10 i)

lookAndSay'' :: [Char] -> [Char]
lookAndSay'' "" = ""
lookAndSay'' x  = (toBase 10 (length y) ++ [head x]) ++ lookAndSay'' z
                where y = (takeWhile (== head x) x)
                      z = (dropWhile (== head x) x)

-- Opdracht 10
keithGetallen :: [Int]
keithGetallen =  concat (map keithGetallen' [10..])

keithGetallen' :: Int -> [Int] 
keithGetallen' i = keithGetallen'' i (toDec i )

keithGetallen'' :: Int -> [Int] -> [Int]
keithGetallen'' i (c:cs) | c > i     = [] 
                         | c == i    = [c]
                         | otherwise = keithGetallen'' i (take l s)
                                     where l = length (c:cs)
                                           s = sum (take l (c:cs)) : (c:cs)

