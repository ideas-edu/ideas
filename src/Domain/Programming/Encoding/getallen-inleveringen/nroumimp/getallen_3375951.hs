--Practicum 1, door Nick Roumimper, studentnummer 3375951.
--Gebruikte compiler: GHCi

--Opgave 1 (fromDec)

fromDec :: [Int] -> Int
fromDec a = foldl (\x y->(10*x)+y) 0 a

--Opgave 2 (toDec)

toDec :: Int -> [Int]
toDec a | a < 10 = [a]
        | otherwise = map convertChar (show a)

--Opgave 3 (fromBin)

fromBin :: [Int] -> Int
fromBin a = foldl (\x y->(2*x)+y) 0 a

--Opgave 4 (toBin)

toBin :: Int -> [Int]
toBin a | a < 2 = [a]
        | even a = toBin (a `div` 2)  ++ [0]
        | otherwise = toBin (a `div` 2) ++ [1]

--Opgave 5 (fromBase)

convertChar :: Char -> Int
convertChar a | fromEnum(a)<fromEnum('a') = fromEnum (a)-fromEnum('0')
              | otherwise = fromEnum (a)-fromEnum('a')+10

convertString :: [Char] -> [Int]
convertString a = map convertChar a

fromBase :: Int -> [Char] -> Int
fromBase a b = foldl (\x y->(a*x)+y) 0 (convertString b)

--Opgave 6 (toBase)

convertInt :: Int -> Char
convertInt a | a >= 10 = toEnum(fromEnum('a')+a-10)
             | otherwise = toEnum(fromEnum('0')+a)

toBase :: Int -> Int -> [Char]
toBase a b | b < a = [(convertInt b)]
           | otherwise = toBase a (b `div` a) ++ [(convertInt(b `mod` a))]

--Opgave 7 (numbers)

legitChar :: Char -> Int -> Bool 
legitChar a b | (convertChar a) >= b = False
		      | otherwise = True

legitTest :: [Char] -> Int -> Bool
legitTest a b = foldr (&&) True (map (`legitChar` b) a)

numbers :: Int -> [String] -> [(String, Int)]
numbers a b | b == [] = []
            | legitTest (head b) a = (head b, fromBase a (head b)):numbers a (tail b)
            | otherwise = numbers a (tail b)

--Opgave 8 deel 1 (deze opgave heb ik helaas niet volledig kunnen afmaken...)

fromGray :: Int -> String -> Bool -> Int
fromGray b s r | (length s <= 1)&&(not (even (convertChar (head s) `div` b))) = b-(convertChar (head s))
               | (length s <= 1)&&(even (convertChar (head s) `div` b)) = convertChar (head s)
               | r = ( (b^((length s)-1)) * (b-1-(convertChar (head s))) )+(fromGray b (tail s) ((not.even) (x `div` (b^(length s-1)))     ))
               | not r = ( (b^((length s)-1)) * ((convertChar (head s))) )+(fromGray b (tail s) ((not.even) (y `div` (b^(length s-1)))     ))
			   where x = ( (b^((length s)-1)) * (b-1-(convertChar (head s))) )
			         y = ( (b^((length s)-1)) * ((convertChar (head s))) )
			   
			   --not(even(convertChar (head s))))))
			   
grayTest :: Int -> String -> Bool -> Int
grayTest b s r | r = ( (b^((length s)-1)) * (b-1-(convertChar (head s))) )
               | otherwise = ( (b^((length s)-1)) * ((convertChar (head s))) )

fromgrayList :: Int -> Int -> [Int]
fromgrayList e b = (fromGray b (grayMax e b) False):(fromgrayList (e+1) b)
			   

--Opgave 8 deel 2 (deze opgave heb ik helaas niet volledig kunnen afmaken)

grayChar :: Int -> Int -> Int -> Char
grayChar e b c | (not.even) (e `div` (b^(c+1))) = convertInt (((b-1)-(e `div` (b^c))) `mod` b)
               | otherwise = convertInt ((e `div` (b^c)) `mod` b)

grayString :: Int -> Int -> Int -> [Char]
grayString e b h | h == 0 = [grayChar e b h]
                 | otherwise = ((grayChar e b h) : (grayString e b (h-1)))

y :: Int -> Int -> Int
y e b = ( until (x e b) (+1) 0)
        where x e b d | e == 0 = True
                      | otherwise = ((b^d)<=e) &&  ((b^(d+1))>e) 

grayMax :: Int -> Int -> [Char]
grayMax e b = grayString e b (y e b)

grayList :: Int -> Int -> [String]
grayList e b = grayMax e b : grayList (e+1) b

--Opgave 9 (lookAndSay)

firstMatch :: String -> Int -> String
firstMatch a b | b == length a = show b
               | a !! b == head a = firstMatch a (b+1)
               | otherwise = show b

fullMatch :: String -> String
fullMatch a | a == "" = ""
            | otherwise = x ++ (take 1 (a)) ++ fullMatch (drop (fromBase 10 x) a)
              where x = firstMatch a 0
			  
repeatAndSay :: String -> [String]
repeatAndSay a = (y):repeatAndSay(y)
               where y = fullMatch (a)

lookAndSay :: Int -> [String]
lookAndSay a = (show a):(y):repeatAndSay(y)
               where y = fullMatch (show a)

--Opgave 10 (keithGetallen)

keithTest :: [Integer] -> Integer -> Bool
keithTest a b | z > b = False
              | z == b = True
              | otherwise = keithTest (drop 1 ( a ++ [z] )) b
			  where z = sum a

toDec2 :: Integer -> [Integer]
toDec2 a | a < 10 = [a]
         | otherwise = map toInteger (map convertChar (show a))
			  
keithResult :: Integer -> [Integer]
keithResult a | keithTest (toDec2 a) a = a:(keithResult (a+1))
              | otherwise = (keithResult (a+1))
			  
keithGetallen :: [Integer]
keithGetallen = keithResult 10

--