--Arno Pol en Kasper van Voorst
--Compiler: GHCi 6.8.3


module PO_Apol_Kjdvoors where
import Char
import List


{-opdracht 1-}
fromDec :: (Num a) => [a] -> a
fromDec a = foldl (helpfunctie) 0 a
	    where x `helpfunctie` y = x*10 + y


{-opdracht 2-}
toDec :: Integer->[Integer]
toDec  a   | a ==0 = []
	   |otherwise       =  toDec ((a- (q)) `div` 10)  ++ [q] 
             where q = a `mod` 10 

{-opdracht 3-}

fromBin :: [Int] -> Int
fromBin [] = 0
fromBin (x : []) = x
fromBin (x : xs) = ( x * 2^length xs ) + fromBin xs

{-Opdracht 4-}

toBin :: Int -> [Int]
toBin 0 = []
toBin x = toBin (quot x 2) ++ [mod x 2]

{-opdracht 5-}

fromBase :: Int -> [Char] -> Int
fromBase z [] = 0
fromBase z (x:xs) = hulpvar5 + fromBase z xs
                    where hulpvar5 = hulpfunc5 z (toLower x) * (z ^ length xs)

hulpfunc5 :: Int -> Char -> Int
hulpfunc5 z x = if isDigit x then digitToInt x else if ((ord x - 86) <= z) then (ord x - 87) else 0

{-Opdracht 6-}

toBase :: Int -> Int -> [Char]
toBase z 0 = []
toBase z y = toBase z (quot y z) ++ result
             where result = [hulpFunc6(mod y z)]

hulpFunc6 :: Int -> Char
hulpFunc6 x = if x > 9 then chr (x+87) else chr (x+48)

{-Opdracht 7-}

numbers :: Int -> [String] -> [(String, Int)]
numbers _ []       = []
numbers x (y : ys) = if hulpfunc7 x y then (y, fromBase x y) : numbers x ys else numbers x ys

hulpfunc7 :: Int -> String -> Bool
hulpfunc7 x [] = True
hulpfunc7 x (y:ys) = ord y - 87 <= x && hulpfunc7 x ys


--Werkt net niet zoals het zou moeten; in plaats van "take x(lookAndSay y)" is de syntax "take x(lookAndSay [y])"

{-Opdracht 10-}
helpfunc101  a n | n<(length a) = a!!n
                 |otherwise     =  help a n (length(a))
                                   where help a n s | s==0     = 0
                                                    |otherwise = (help a (n-1) (s-1)) +  helpfunc101 a (n-1)

keithGetallen a  = helpfunc102 a 0 where  helpfunc102 a n = helpfunc101 (toDec a) n : helpfunc102 a (n+1)

--werkt net niet zoals het zou moeten; in plaats van "take x keithgetallen" is de syntax "take 10(keithgetallen x)"
