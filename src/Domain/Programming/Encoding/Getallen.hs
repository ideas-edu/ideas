module Getallen where

import Char

fromBin :: [Int] -> Int
fromBin = foldl ((+) . (* 2)) 0

toBin :: Int -> [Int]
toBin 0 = [0]
toBin n = to n []
  where
    to 0 = id
    to n = let (n', k) = n `divMod` 2
           in  to n' . (k :)

fromDec :: [Int] -> Int
fromDec = foldl ((+) . (* 10)) 0

toDec :: Int -> [Int]
toDec 0 = [0]
toDec n = to n []
  where
    to 0 = id
    to n = let (n', k) = n `divMod` 10
           in  to n' . (k :)

fromDigit :: Char -> Int
fromDigit c | c >= '0' && c <= '9' = ord c - ord '0'
            | c >= 'a' && c <= 'z' = ord c - ord 'a' + 10

toDigit :: Int -> Char
toDigit n | n >= 0  && n <= 9  = chr (ord '0' + n)
          | n >= 10 && n <= 35 = chr (ord 'a' + n - 10)

fromBase :: Int -> [Char] -> Int
fromBase b = foldl ((. fromDigit) . (+) . (* b)) 0

toBase :: Int -> Int -> [Char]
toBase _ 0 = ['0']
toBase b n = to n []
  where
    to 0 = id
    to n = let (n', k) = n `divMod` b
           in  to n' . (toDigit k :)

numbers :: Int -> [String] -> [(String, Int)]
numbers b css = [(cs, fromBase b cs) | cs <- css, all inRange cs]
  where
    inRange c =
      (c >= '0' && c <= '9' || c >= 'a' && c <= 'z') && c <= toDigit (b - 1)

grayCode :: Int -> ([Char] -> Int, Int -> [Char])
grayCode b = (fromGray b, toGray b)

fromGray :: Int -> [Char] -> Int
fromGray b = fst . foldl f (0, False)
  where
    f (acc, inv) d | inv       = (b * acc + b - n - 1, even n)
                   | otherwise = (b * acc + n, odd n)
                   where n = fromDigit d

toGray :: Int -> Int -> [Char]
toGray _ 0 = ['0']
toGray b n = fst (to n) []
  where
    to 0 = (id, False)
    to n = let (n', k)    = n `divMod` b
               (acc, inv) = to n'
               (k', inv') = if inv then (b - k - 1, even k') else (k, odd k)
           in  (acc . (toDigit k' :), inv')

lookAndSay :: Int -> [String]
lookAndSay n = say
  where
    say = toBase 10 n : map (look 1) say
    look acc []                            = ""
    look acc [c]                           = toBase 10 acc ++ [c]
    look acc (c : cs@(c' : _)) | c == c'   = look (acc + 1) cs
                               | otherwise = toBase 10 acc ++ [c] ++ look 1 cs

keith :: Int -> [Int]
keith n = ns
  where
    ns = ds ++ [sum ds] ++ zipWith ((-) . (* 2)) (drop (length ds) ns) ns
    ds = toDec n

keithGetallen :: [Int]
keithGetallen = [n | n <- [10 ..], head (dropWhile (< n) (keith n)) == n]