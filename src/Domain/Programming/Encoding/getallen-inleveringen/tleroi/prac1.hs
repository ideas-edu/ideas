-- Tijmen Leroi, Studentnummer 3169774
-- Compiler GHC


-- 1. fromDec [ 4, 2, 3 ] geeft 423

fromDec :: [Int] -> Int
fromDec2 :: [Int] -> Int -> Int

fromDec x = fromDec2 x ((length x) - 1)

fromDec2 (a:[]) _ = a 
fromDec2 (h:t) x = (h * (10 ^ x)) + fromDec2 t (x - 1)


-- 2. toDec 423 geeft [ 4, 2, 3 ]

toDec :: Int -> [Int]
toDec2 :: Int -> [Int]

toDec a = reverse (toDec2 a)

toDec2 0 = []
toDec2 a = b: (toDec2 (div (a - b) 10))
		where b = mod a 10


-- 3. fromBin [1,0,0,1,1,0] geeft 38

fromBin :: [Int] -> Int
fromBin2 :: [Int] -> Int -> Int

fromBin x = fromBin2 x ((length x) - 1)

fromBin2 (a:[]) _ = a 
fromBin2 (h:t) x = (h * (2 ^ x)) + fromBin2 t (x - 1)


-- 4. toBin 423 geeft [ 4, 2, 3 ]

toBin :: Int -> [Int]
toBin2 :: Int -> [Int]

toBin a = reverse (toBin2 a)

toBin2 0 = []
toBin2 a = b: (toBin2 (div (a - b) 2))
		where b = mod a 2

		
-- 5. fromBase 10 "423" geeft 423; fromBase 2 "100110" geeft 38; 
--    fromBase 16 "f4c8" geeft 62664; fromBase 23 "5j" geeft 134

toInt :: Char -> Int
toInt x
  | x >= '0' && x <= '9' =  fromEnum x - fromEnum '0'
  | x >= 'a' && x <= 'z' =  fromEnum x - fromEnum 'a' + 10
  | x >= 'A' && x <= 'Z' =  fromEnum x - fromEnum 'A' + 10
  | otherwise            =  error "Char.toInt: not a digit"


fromBase :: Int -> [Char] -> Int
fromBase2 :: Int -> [Char] -> Int -> Int


fromBase z x = fromBase2 z x ((length x) - 1)

fromBase2 _ (a:[]) _ = (toInt a) 
fromBase2 z (h:t) x = ((toInt h) * (z ^ x)) + fromBase2 z t (x - 1)


-- 6. toBase 10 423 geeft "423"; toBase 2 38 geeft "100110";
--    toBase 16 62664 geeft "f4c8"; toBase 23 134 geeft "5j"

fromInt :: Int -> Char
fromInt x
  | x >= 0  && x <=  9   =  toEnum (fromEnum '0' + x)
  | x >= 10 && x <= 35   =  toEnum (fromEnum 'a' + x - 10)
  | otherwise            =  error "Char.fromInt: not a digit"

toBase :: Int -> Int -> [Char]

toBase a b = reverse (toBase2 a b)

toBase2 _ 0 = []
toBase2 x a = (fromInt b): (toBase2 x (div (a - b) x))
		where b = mod a x
		
-- 7. numbers 16 [ "ace", "face", "faces" ] geeft [ ("ace", 2766), ("face", 64206)]

numbers :: Int -> [String] -> [(String,Int)]
numbers _ [] = []
numbers a (b:c) = ((b,(fromBase a b)):(numbers a c))

-- 8.  
{-

grayCode :: Int -> ([Char] -> Int,Int -> [Char])
fromGray :: Int -> [Char] -> Int
toGray   :: Int -> Int -> [Char]

grayCode a = (fromGray a, toGray a)

0    0
1    1
2    2
3   02
4   12
5   22
6  022
7  122
8  222
9 0222
-}

-- 9. take 6 (lookAndSay 1) geeft [ "1", "11", "21", "1211", "111221", "312211" ]

lookAndSay :: Int -> [String]

lookAndSay a = (toBase 10 a):(lookAndSay2 a)
lookAndSay2 a = (toBase 10 y):(lookAndSay2 y)
				where y = (fromDec (lookAndSay3 (toDec a)))
				
lookAndSay3 [] = []
lookAndSay3 (x:xs)	= (length (takeWhile (==x) (x:xs))):x:(lookAndSay3 (dropWhile (==x) (x:xs)))
