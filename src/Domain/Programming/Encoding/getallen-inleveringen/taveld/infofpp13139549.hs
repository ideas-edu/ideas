{-
 functional programming practicum 1
T.A. in 't Veld, studentnr 3139549
using GHC 6.11
-}

module P1 where
	import Char
		
	
	-- opgave 1
	fromDec:: (Integral a) => [a] -> a
	fromDec (x:xs) = x *10^(length(xs)) + fromDec xs
	fromDec _ = 0


	-- opgave 2
 	toDec:: (Integral a) =>  a -> [a]
	toDec x = doToDec x 1 where
		doToDec x y  	| x <= 0 = []
				| (x > 0) = (doToDec (x - x `mod` factor) (y * 10)) ++ [(x `mod` factor) `div` y] where
			factor = 10 * y

	-- opgave 3
	fromBin :: [Int ] -> Int
	fromBin x = doFromBin (reverse x) 0  where
		doFromBin (y:ys) z = y * 2^z + doFromBin ys (z +1)
		doFromBin [] _ = 0 

	-- opgave 4
	
	toBin x = doToBin x 1 where
		doToBin x y  	| x <= 0 = []
				| (x > 0) = (doToBin (x - x `mod` factor) (y * 2)) ++ [(x `mod` factor) `div` y] where
				factor = 2 * y	

	-- helper functions for remaining exercises 



	


	-- opgave 5
	fromBase :: Int -> [Char] -> Int
	fromBase x (y:ys)   = getNumber y *x^(length ys)+ fromBase x ys where
		getNumber z | ord z > 47 && ord z < 58 =   ord z - 48
		    	    | ord z > 96 && ord z < 123 = ord z - 87  
		
	fromBase _ _ = 0

-- opgave 6 
	toBase::  Int -> Int -> [Char]
	toBase b x = doToBase x 1 where
		doToBase x y  	| x <= 0 = []
				| (x > 0) = (doToBase (x - x `mod` factor) (y * b)) ++ [getCharacter((x `mod` factor) `div` y)] where
				factor = b * y	
				getCharacter x | x >= 0  && x <= 9  = chr(x + 48)
	       	       				| x >= 10 && x <= 36 = chr (x + 87)


	-- opgave 7 
	numbers b x = doNumbers x where
		doNumbers[] = []
		doNumbers (x:xs) | correct x = (x, fromBase b x) : doNumbers xs
				 | otherwise = doNumbers xs  where
			correct (y:ys) | fromBase b [y] <= b = correct ys
					| otherwise = False
			correct [] = True
{-
	-- opgave 8
	grayCode b = (fromGrayCode, toGrayCode) where
		fromGrayCode = 


 -- opgave 8
-}		
-- opgave 9
{-

	lookAndSay x = [numString x] ++    (doLAS ([numString x]) []) where
		
		doLAS [] f = doLAS f []  
		doLAS (y:ys) z = res  where 
			res  = [numString (repeats (y:ys))] ++ [y]

-}
	lookAndSay z = numString z : doLAS (numString z) where 
		numString g = toBase 10 g
		doLAS y =   [describe y] ++ doLAS (describe y) ++ [""] where 
			describe [] = ""
			describe (x:xs) = numString (repeats (x:xs))  ++ [x] ++ describe (remains x xs) where 
				repeats (a:b:as) | a == b = 1 + repeats (b:as)
					 	| a /= b = 1
				repeats _ = 1
				remains c (d:ds) | c == d = remains c ds
					 	| otherwise = d:ds 
				remains _ e = e


	
	
	-- opgave 10
	keithGetallen:: [Integer]
	keithGetallen = [] ++ doKG 14 where 
		doKG x | isKeith x (toDec x) = [x] ++ doKG (x + 1)
	       	       | otherwise = doKG (x + 1) where 
			isKeith y z  | sum z < y = isKeith y (keithAdvance z)
				     | sum z == y = True
				     | sum z > y = False where 
				keithAdvance (x:xs) = xs ++ [sum (x:xs)]