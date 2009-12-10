-- Functioneel programmeren --
-- Practicum 1     Getallen --
-- Arne de Both 	3220478 --
-- Lambert Veerman	3233596 --


import Char

-- hulpfunctie
fromBase' :: Int -> [Int] -> Int
fromBase' _ []     = 0
fromBase' n (x:xs) = x * n ^ length xs + fromBase' n xs

-- Opdracht 1
fromDec :: [Int] -> Int
fromDec = fromBase' 10 

-- Opdracht 2
toDec :: Int -> [Int]
toDec 0 = [0]
toDec x = toDec' [] x 
	      where toDec' t 0 = t
	            toDec' t y =  toDec' ((y `mod` 10) : t) (y `div` 10)

-- Opdracht 3
fromBin :: [Int] -> Int
fromBin = fromBase' 2

-- Opdracht 4
toBin :: Int -> [Int]
toBin 0 = [0]
toBin x = toBin' [] x 
	      where toBin' t 0 = t
	            toBin' t y =  toBin' ((y `mod` 2) : t) (y `div` 2)

-- Opdracht 5
fromBase :: Int -> [Char] -> Int
fromBase _ [] = 0
fromBase n c  =  fromBase' n (map digitValue c)

-- hulpfunctie                
digitValue :: Char -> Int
digitValue c | ord c >= ord 'a' = ord c - ord 'a' + 10
             | otherwise        = ord c - ord '0'

-- Opdracht 6
toBase :: Int -> Int -> [Char]
toBase _ 0 = "0"
toBase n x = toBase' n [] x 
	where toBase' :: Int -> [Char] -> Int -> [Char]
	      toBase' _ t 0 = t
	      toBase' n t y =  toBase' n (charValue (y `mod` n) : t) (y `div` n)	

-- hulpfunctie
charValue :: Int -> Char
charValue x | x < 10 	  = chr (ord '0' + x)
	          | otherwise	= chr (ord 'a' + x - 10)

-- Opdracht 7
numbers :: Int -> [String] -> [(String,Int)]
numbers _ [] = []
numbers n (c:cs) | and (map ((<n) . digitValue) c) = (c, fromBase n c) : numbers n cs
		             | otherwise                       = numbers n cs

-- Opdracht 8
grayCode :: Int -> ([Char] -> Int, Int -> [Char])
grayCode n = (fromGray n, toGray n)

-- hulpfunctie
toGray :: Int -> Int -> [Char]
toGray n x = map charValue (toGray' n base 0)
	         where base = map digitValue (toBase n x)

toGray' :: Int -> [Int] -> Int -> [Int]
toGray' _ [] _ = []
toGray' n (x:xs) s = gray:(toGray' n xs (s+gray))
	                 where gray = (x+n-s) `mod` n

-- hulpfunctie
fromGray :: Int -> [Char] -> Int
fromGray n c = fromDec (reverse (fromGray' n base (foldl (+) 0 base)))
	           where base = reverse (map digitValue c)

fromGray' :: Int -> [Int] -> Int -> [Int]
fromGray' _ [] _ = []
fromGray' n (x:xs) s = (snew+x):(fromGray' n xs snew)
	                   where snew = s-x
 
-- Opdracht 9 
lookAndSay :: Int -> [String]
lookAndSay x = [xs] ++ lookAndSay' xs
             where xs = toBase 10 x
                   lookAndSay' :: String -> [String]  
                   lookAndSay' s = nexts : lookAndSay' nexts 
                                 where nexts = next 0 '0' s
                                      
-- hulpfunctie
-- reken volgende getal in lookAndSay reeks uit
next :: Int -> Char -> String -> String
next _ _ []                   = []
next i c (a:[])               = charValue (i+1) : a : []
next i c (a:b:ss) | a == b    = next (i+1) a (b:ss)
                  | otherwise = charValue (i+1) : a : next 0 b (b:ss)
                              
 
-- Opdracht 10
keithGetallen :: [Int]
keithGetallen = filter isKeith [10..]

-- hulpfunctie 
-- Is een getal een keithGetal?
isKeith :: Int -> Bool
isKeith x = f x (keithReeks x)
          where f :: Int -> [Int] -> Bool
                f e (x:xs) | e == x   = True
                           | e <  x   = False
                           |otherwise = f e xs 

-- hulpfunctie 
-- Geef de hele Keithreeks van een decimaal getal
-- Geef in de recursie telkens maar n elementen mee 
keithReeks :: Int -> [Int]
keithReeks x = xs ++ k' (length xs) xs
              where xs = toDec x
                    k' :: Int -> [Int] -> [Int]
                    k' l xs = elem : k' l (tail xs ++ [elem])
                            where elem = sum ( drop (length xs - l) xs )

