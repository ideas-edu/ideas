-- Geschreven in Notepad++, gecompileerd met Helium.

fromDec::[Int]->Int
fromDec (a : as) = a*(10^(length(a : as)-1)) + fromDec(as)
fromDec [] = 0

--toDec::Int->[Int]
--toDec a = toDec' a ++ []

--toDec'::Int->[Int]
--toDec' a = toDec' (a `div` 10) : a `rem` 10
--toDec' 0 = []

fromBin::[Int]->Int
fromBin (a : as) = a*(2^(length(a : as)-1)) + fromBin(as)
fromBin [] = 0

--toBin::Int->[Int]
--toBin a = 

--fromBase::Int->[Char]->Int
--fromBase n (a : as) = a*(n^(length(a : as)-1)) + fromBase n as

--toBase::Int->Int->[Char]
--toBase n a = 

--numbers::Int->[String]->[(String, Int)]
--numbers n a = 

--grayCode::Int->([Char]->Int, Int->[Char])
--grayCode a = 

--lookAndSay::Int->[String]
--lookAndSay a = 

--keithGetallen::[Int]
--keithGetallen = 