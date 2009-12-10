
-- Practicum 1 -- Max van Boxel & Matthijs Venselaar
-- Gebruikte compiler = helium

-- Opgave 1 -- 
fromDec :: [Int] -> Int
fromDec x = maakGetal 10 x 0

-- Opgave 2
toDec :: Int -> [Int]
toDec x = reverse (maakLijst 10 x)

-- Opgave 3
fromBin :: [Int] -> Int
fromBin x = maakGetal 2 x 0

-- Opgave 4
toBin :: Int -> [Int]
toBin x = reverse (maakLijst 2 x )

-- Opgave 5
fromBase :: Int -> [Char] -> Int
fromBase _ [] = 0
fromBase b x = if (b > 36 || b < 0) then 0 else maakGetal' b x 0
   
-- Opgave 6 -- 
toBase :: Int -> Int -> [Char]
toBase a b = if (a > 36 || a < 0) then [] else reverse ((maakLijst' a b))

-- Opgave 7 --
numbers :: Int -> [String] -> [(String,Int)]
numbers _ [] = []
numbers a (x : xs) = if (a > 10) then if (y == 0) then numbers a xs else (x, y) : (numbers a xs)
                     else []
                     where y = (fromBase a x)

-- Opgave 8 --
grayCode :: Int -> ([Char] -> Int, Int -> [Char])
grayCode a = if (a>=2 && a<=36) then  (grayToInt a,grayToChar a) else (error "",error "")

-- Opgave 9 --
lookAndSay :: Int -> [String]
lookAndSay x = fromIntoString y : lookAndSay (fromDec (lookAndSay' y))
               where y = toDec x

-- Opgave 10 --
keithGetallen :: [Int]
keithGetallen = keithGetallen2 [11..]




-- Opgave 1 & 3 -- Extra hulpfunctie
maakGetal :: Int -> [Int] -> Int -> Int
maakGetal _ [] b = b
maakGetal a (x:xs) b = if (x < a) then  maakGetal a xs (b * a + x) else 0

-- Opgave 2 & 4 -- Extra hulpfunctie
maakLijst :: Int -> Int -> [Int]
maakLijst _ 0  = []
maakLijst a x = y : (maakLijst a ((x - y) `div` a))
          where y = x `mod` a

-- Opgave 5 -- Extra hulpfunctie
maakGetal' :: Int -> [Char] -> Int -> Int
maakGetal' _ [] b = b
maakGetal' a (x:xs) b = if (y > (a - 1)) then 0 else maakGetal' a xs (b * a + y)
                       where y = if (fromEnum x) > 96 then ((fromEnum x)-87) else ((fromEnum x) - 48)

-- Opgave 6 -- Hulpfunctie
maakLijst' :: Int -> Int -> [Char]
maakLijst' _ 0 = []
maakLijst' a b = (toEnum (if (y > 10) then y + 87 else y + 48)::Char) : maakLijst' a((b - y) `div` a)
              where y = b `mod` a



-- Opgave 8 -- Hulpfuncties

-- Deze functie maakt een Graycode van een getal
grayToChar :: Int -> Int -> [Char]
grayToChar _ 0 = [(toEnum 48)::Char]
grayToChar a b = intToChar(grayToChar' a 0 (reverse(maakLijst a b)))   

-- Hulpfunctie die een lijst van Ints in een stringomzet
intToChar :: [Int] -> [Char]
intToChar [] = []
intToChar (x:xs) = ((if(x>9) then toEnum (x+87) else toEnum (x+48)) : []) ++ intToChar xs

-- Hulpfunctie voor grayToChar
grayToChar' :: Int -> Int -> [Int] -> [Int]
grayToChar' _ _ [] = []
grayToChar' a b (x:xs) = y : [] ++ grayToChar' a (b+y) (xs)
             where y = (x+a-b)`mod`a    

-- Deze functie levert van een Graycode het bijbehorende getal
grayToInt :: Int -> [Char] -> Int
grayToInt _ [] = 0
grayToInt a (x:xs) = searchInt a (x:xs) 0

-- Hulpfunctie voor grayToInt
searchInt :: Int -> [Char] -> Int -> Int
searchInt _ [] _ = 0
searchInt a (x:xs) b = if((grayToChar a (y))==(x:xs)) then y else searchInt a (x:xs) (b+1)
             where y = (a^(length(x:xs)-1))+b





-- Opgave 9 -- Hulpfuncties

fromIntoString :: [Int] -> String
fromIntoString [] = []
fromIntoString (x:xs) =  (toEnum(x+48)::Char) : fromIntoString xs

lookAndSay' :: [Int] -> [Int]
lookAndSay' [] = []
lookAndSay' (x:xs) = y ++ lookAndSay'(drop (head y) (x:xs))
        where y = (lookAndSay''(x:xs))

lookAndSay'' :: [Int] -> [Int]
lookAndSay'' [] = []
lookAndSay'' (x:xs) = (length(takeWhile p xs) + 1) : x : []
              where p y = (x == y)








-- Opgave 10 -- Hulpfuncties

keithGetallen2 :: [Int] -> [Int]
keithGetallen2 [] = []
keithGetallen2 (x:xs) = if (keithGetallen' x (toDec x)) then x : keithGetallen2 xs else keithGetallen2 xs

keithGetallen' :: Int -> [Int] -> Bool
keithGetallen' _ [] = False
keithGetallen' a (x:xs) = if (a == x) then True else if (a < x) then False else keithGetallen' a (xs ++ [y])
                       where y = foldr (+) 0 (x:xs)
