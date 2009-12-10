
module Getallen where
import Char

-- Naam          : Ederlyn Gallo
-- Student nummer: 3083209
-- Compiler      : GHCI  6.10.1

---- Getallen systemen
------------------------------- Opgave 1 -----------------------------------
fromDec :: [Int] -> Int
fromDec = foldl (\r n -> 10 * r + n) 0

------------------------------- Opgave 2 -----------------------------------
toDec   :: Int -> [Int]
toDec 0   = [0]
toDec n   = toB n 10 []

-- this function converts a given number to its equivalent base value
toB :: Int -> Int -> [Int]-> [Int]
toB 0 b r = r
toB n b r = toB l b (d:r)
       where l  = n `div` b
             d  = n `mod` b

------------------------------- Opgave 3 -----------------------------------
fromBin :: [Int] -> Int
fromBin   = foldl (\r n -> 2 * r + n) 0

------------------------------- Opgave 4 -----------------------------------
toBin   :: Int -> [Int]
toBin 0   = [0]
toBin n   =  toB n 2 []

------------------------------- Opgave 5 -----------------------------------
fromBase :: Int -> [Char] -> Int
fromBase n ns   = foldl (\r x -> n * r + x) 0 (convert ns (n))

-- help function that converts the given [Char] to its equivalent number
-- the letters to be used by a given base is controlled here
-- for example, numbers of base 16 use only letters a-f
-- or numbers of base 2 use only 0,1
-- invalid numbers has result 0

convert :: [Char] -> Int -> [Int]
convert []         n    = []
convert cs n
        |validNum  n cs = convert' cs  n
        |otherwise      = []

convert' :: [Char] -> Int -> [Int]
convert' []     n       = []
convert' (c:cs) n
      |isDigit  c       = (ord c - ord '0'):convert' cs n
      |isAlpha  c       = if (not p) then []
                               else (n2 ? toLower c):convert' cs  n
           where  n2    = zip l [10..]
                  l     = take n ['a'..'z']
                  p     = elem c l


validNum :: Int -> [Char] -> Bool
validNum b []         = True
validNum b (c:cs)
         |isDigit c   = elem n d && validNum b cs
         |isAlpha c   = elem c l && validNum b cs
       where l        = take x ['a'..'z']
             d        = take b [0..9]
             n        = digitToInt c
             x        = b-10

-- lookup the value that's represented by another given value
(?) :: Eq a =>  [(a,b)] -> a -> b
[] ? x                     = error "not found"
((a,b):ts) ? x | x==a      = b
               | otherwise = ts ? x


------------------------------- Opgave 6 -----------------------------------
toBase :: Int -> Int -> [Char]
toBase _ 0                 = ['0']
toBase b n                 = toBase' b n []

toBase' :: Int -> Int -> [Char] -> [Char]
toBase' b 0 r              = r
toBase' b n r              = toBase' b l (dig:r)
        where l            = n `div` b
              d            = n `mod` b
              dig          = if (d>=10) then locL d (b-10)
                                        else chr(ord '0' + d)

locL :: Int -> Int -> Char
locL n b          =  list ? n
       where list = zip [10..] (take n ['a'..'z'])


------------------------------- Opgave 7 -----------------------------------
-- only numbers of a given base that are valid are processed
numbers :: Int -> [String] -> [(String, Int)]
numbers n [] = []
numbers n css@(c:cs)
    |n < 2 || n > 36     = error "The given base is not within the limit"
    |validNum n c       = (c,v):numbers n cs
    |not (validNum n c) = numbers n cs
            where   v    = fromBase n c

-- this filters the valid numbers given its base
filterVal :: Int -> [String] -> [String]
filterVal _ []           = []
filterVal n (c:cs)
    |validNum n c        = c:filterVal n cs
    |otherwise           = filterVal n cs



---- Gray-coderingen
------------------------------- Opgave 8 -----------------------------------

{-  this problem is solved according to this algorithm
   1. Start with the Gray code sequence "0, 1"
   2. Make a copy of the sequence.
   3. Reverse the order of the copy.
   4. Prepend a "0" to each number in original sequence.
   5. Prepend a "1" to each number in the copy.
   6. Concatenate the original and the copy together to create a new Gray code sequence twice as long as the original.
   7. If you want more bits worth of Gray code, go back to step 2.
-}
-- test functions
fromGray n cs = (fst $ grayCode n) cs
toGray n  x = (snd $ grayCode n) x

grayCode :: Int -> ([Char] -> Int, Int -> [Char])
grayCode n               = (fromGray, toGray)
      where toGray       = (?) (zip [0..] (prodGray n))
            fromGray     = (?) (zip (prodGray n) [0..])
            
-- this function produces the Gray codes for a given base
prodGray :: Int -> [String]
prodGray n               = subGray ++ (prodGray' nums subGray)
       where nums        = produceNum (n-1)
             subGray     =  gray' nums []

prodGray' :: String -> [String] -> [String]
prodGray' ns grs         =  xs ++ (prodGray' ns xs)
       where xs          = gray ns grs

gray :: String -> [String] -> [String]
gray [] _                = []
gray cs []               = []
gray (c:cs) xs           = (gray cs (reverse xs)) ++ (map (f c) xs)
       where f c cs      = c:cs

-- this function produces the sub gray-code
gray' :: String -> String -> [String]
gray' [] _               = []
gray' cs []              = gray' cs cs
gray' (c:cs) xs          = (append c xs )++ (gray' cs (reverse xs))

append :: Char -> String -> [String]
append n []              = []
append n (x:xs)          = c:append n xs
       where c           = n:[x]

produceNum :: Int -> String
produceNum 0 = "0"
produceNum n
    |n > 9               = produceNum(n-1)++[conv n]
    |otherwise           = produceNum (n-1)++ [intToDigit n]
       where conv n      = (zip [10..] ['a'..'z']) ? n

---- Look-and-say-reeksen
------------------------------- Opgave 9-----------------------------------
lookAndSay :: Int -> [String]
lookAndSay n              = start: (look num)
       where start        = map intToDigit num
             num          = toDec n

look :: [Int] -> [String]
look []                   = []
look xs                   = start: (look xss)
       where start        = tally $ group xs
             xss          = map digitToInt start


-- this function groups same consecutive entries
group :: [Int] -> [[Int]]
group []                  = []
group xss@(x:xs)          = first : group rest
       where first        = takeWhile (==x) xss
             rest         = dropWhile (==x) xs

-- this function keeps track of the number of appearances of a given entry
-- their Int values are then converted to Char and then concatenated
tally :: [[Int]] -> String
tally xs                  = concatMap append (map pair xs)
       where append (x,y) = x:[y]
             pair z       = (l,z')
               where l    = intToDigit (length z)
                     z'   = intToDigit (head z)

---- Keith-getallen
------------------------------- Opgave 10 -----------------------------------
keithGetallen :: [Integer]
keithGetallen             = kNums 10
       where kNums n
               |inRange n = n:kNums (n+1)
               |otherwise = kNums (n+1)


-- this function checks if the given number appears in the range
inRange :: Integer -> Bool
inRange n
    |n == hd              = True
    |otherwise            = False
       where hd           = head $ dropWhile (< n) (range n 0)


-- this function produces the range of a given number n
range :: Integer -> Int -> [Integer]
range n i                 = ns ++ (range' ns i)
       where ns           = toDec' n

range' :: [Integer] -> Int -> [Integer]
range' xs i               = [s] ++ (range' hds (i+1))
       where ds           = drop i xs
             s            = sum ds
             hds          = xs ++ [s]

-- this function is same as that of toDec above except that the type Integer (which is not needed by the previous problems) is being used here
-- to produce more keith-numbers by the keithGetallen function
toDec'   :: Integer -> [Integer]
toDec' 0                  = [0]
toDec' n                  = toB' n 10 []

toB' :: Integer -> Integer -> [Integer]-> [Integer]
toB' 0 b r                = r
toB' n b r                = toB' l b (d:r)
       where l            = n `div` b
             d            = n `mod` b

 --------------------------------------------- E  N  D ----------------------------------------------