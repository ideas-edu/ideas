module DeHaan_3228576 where

import Char

-- Functional Programming Course
-- Ronald de Haan -- 3228576
-- First practicum

-- generic converter for integer bases
fromBaseInt :: Int -> [Int] -> Int
fromBaseInt n = (foldr (\x y -> x + n*y) 0) . reverse

toBaseInt :: Int -> Int -> [Int]
toBaseInt n c = reverse (map (`rem` n) (takeWhile (/=0) (iterate (`div` n) c) ) )

-- opgave 1
fromDec :: [Int] -> Int
fromDec = fromBaseInt 10

-- opgave 2
toDec :: Int -> [Int]
toDec = toBaseInt 10

-- opgave 3
fromBin :: [Int] -> Int
fromBin = fromBaseInt 2

-- opgave 4
toBin :: Int -> [Int]
toBin = toBaseInt 2

-- converters from integers to corresponding characters (0~'0', ... 35~'z')
numToChar :: Int -> Char
numToChar n | (0 <= n) && (n <= 9)  = chr (n+48)  -- cases 0-9
            | (n > 9)  && (n <= 35) = chr (n+87)  -- cases a-z
            | otherwise             = chr 0       -- error case

charToNum :: Char -> Int
charToNum c | ('0' <= c) && (c <= '9')  = (ord c)-48  -- cases 0-9
            | (c > '9')  && (c <= 'z')  = (ord c)-87  -- cases a-z
            | otherwise                 = -1          -- error case

-- opgave 5
fromBase :: Int -> [Char] -> Int
fromBase n | (2 <= n) && (n <= 35) = (foldr (\x y -> (charToNum x) + n*y) 0) . reverse
            | otherwise             = (\x -> 0)
            
-- opgave 6
toBase :: Int -> Int -> [Char]
toBase _ 0 = "0"
toBase n c | (2 <= n) && (n <= 35) = reverse (map (numToChar.(`rem` n)) (takeWhile (/=0)
                                                                             (iterate (`div` n) c) ) )
            | otherwise             = []

-- opgave 7
numbers :: Int -> [String] -> [(String,Int)]
numbers b xs = filter f (map (\x -> (x, fromBase b x)) xs) where
               f ((x:xs),y) = (not (elem x [a..'z'])) && (f (xs,y)) where
                      a = numToChar b
               f ([],y) = True

-- function recursively enumerating n-ary gray codes
-- for a specified number of iterations (length of list is base^(number of iterations) ).
baseGray :: Int -> Int -> [[Int]]
baseGray b 0 = [[0]]
baseGray b x = repTimes x next [[]]
                where
                next xs = concat (zipWith f (take b (repeat xs)) (baseset))
                baseset = take b (iterate (+1) 0)
                f xs y | even y    = map (y:) xs
                       | otherwise = map (y:) (reverse xs)

-- function repeating a certain function a certain amount of times
repTimes :: Int -> (a -> a) -> a -> a
repTimes 0 _ x = x
repTimes n f x = repTimes (n-1) f (f x)

-- function estimating a root function conservatively: b to the power (upperRoot b x) > x
upperRoot :: Int -> Int -> Int
upperRoot b x = length (takeWhile (>0) (iterate (`div` b) (x)))

-- function searching the position of an element in a list;
-- returns one plus the length of the list if the element is not found
searchList :: Eq a => [a] -> a -> Int
searchList [] _ = 0
searchList (x:xs) e | x == e    = 0
                    | otherwise = 1 + (searchList xs e)

-- not very efficient implementation of n-ary gray codes,
-- recursively enumerating the whole sequence of gray codes
grayCodeRecur :: Int -> ([Char] -> Int, Int -> [Char])
grayCodeRecur b = (e,g) where
                e xs | (f xs) < (b ^ (length xs)) = f xs
                     | otherwise                      = -1 -- in case of error
                f xs = searchList (map (fromBase b) (map (map numToChar) (baseGray b (length xs)))) (fromBase b xs)
                g n = map (numToChar) ((baseGray b (upperRoot b n)) !! n)

-- opgave 8

-- efficient implementation of n-ary gray codes,
-- using the algorithm proposed in Sharma & Khanna
-- (Sharma, B.D. and R. K. Khanna, “On m-ary Gray codes,” Information Sciences,
-- vol. 15, pp. 31-43, 1978.)
grayCode :: Int -> ([Char] -> Int, Int -> [Char])
grayCode b = (  (\xs -> fromBaseInt b (gCfrom b (toBaseInt b (fromBase b xs)))),
                (\xs -> toBase b (fromBaseInt b (gCto b (toBaseInt b xs))) )  ) where
                  gCto n xs = zipWith (\x y -> (x-y) `mod` n) xs (0:(take ((length xs)-1) xs)) where
                  gCfrom n (x:xs) = x:(f x xs) where
                                        f x [] = []
                                        f x (c:cs) = i:(f i cs) where
                                                         i = ((x+c) `mod` n)

-- opgave 9
lookAndSay :: Int -> [String]
lookAndSay = (iterate once).(toBase 10) where
                   once l | f == []   = []
                                    | g == []   = h
                                    | otherwise = h ++ (once g) where
                                        h = (numToChar (length f)):((head f):[])
                                        f | l == []   = []
                                          | otherwise = takeWhile (==(head l)) l
                                        g | l == []   = []
                                          | otherwise = dropWhile (==(head l)) l


-- opgave 10
keithGetallen :: [Int]
keithGetallen = [ x | x <- [14..], check x (reeks x) ] where
                   check x (y:ys) | (y:ys) == [] = False
                                  | y == x       = True
                                  | y>x          = False
                                  | otherwise    = check x ys
                   reeks n = (toDec n) ++ (h (toDec n)) where
                                h (o:os) = s:(h n) where
                                     s = sum (o:os)
                                     n = reverse (s:(reverse os))

