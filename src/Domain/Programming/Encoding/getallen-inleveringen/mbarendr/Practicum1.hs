{- 
   Practicum 1 
   Auteur: Martijn Barendregt, 3137791 
   Compiler: Helium 
-}

module Practicum1 where

{- Hulpfuncties -}
toBase' :: Int->Int->[Int]
toBase' _ 0 = []
toBase' n getal | n < 2 = []
                | n > 36 = []
                | otherwise = toBase' n (getal/n) ++ [mod getal n]

digitValue :: Char -> Int
digitValue c = ord c - ord '0'

digitChar :: Int -> Char
digitChar n = chr(n + ord '0')

charToInt :: Char->Int
charToInt c | digitValue(c) < 10 = digitValue c
            | otherwise = (digitValue(c) - digitValue('a')) + 10

intToChar :: Int->Char
intToChar n | n < 10 = digitChar n
            | otherwise = chr( (n + ord 'a') - 10)

vanaf :: Int->[Int]
vanaf n = n : vanaf (n + 1)

isKeithGetal :: Int->Bool
isKeithGetal n | n < 10 = False
               | otherwise = or (map (== n) (genKeithGetallen (toDec n) (length (toDec n))))

genKeithGetallen :: [Int]->Int->[Int]
genKeithGetallen l size = genKeithGetallen (l ++ [sum (take size (reverse l))]) size

{- 
   Opgave 1:
   fromDec [Int]->Int
   Geeft een decimaal getal terug, door de invoer met machten van 10 te vermenigvuldigen 
-} 
fromDec :: [Int] -> Int
fromDec [] = 0
fromDec [decimaal] = decimaal
fromDec (decimaal:rest) = decimaal * 10^length rest + fromDec rest

{- 
   Opgave 2:
   toDec Int->[Int]
   Breek een decimaal getal op in de indiviuele decimalen 
-}
toDec :: Int->[Int]
toDec 0 = []
toDec getal = toDec (getal/10) ++ [mod getal 10]

{-
   Opgave 3:
   fromBin [Int]->Int
   Zet een binair getal om in een decimale waarde
-}
fromBin :: [Int]->Int
fromBin [] = 0
fromBin [bin] = bin
fromBin (bin:rest) = bin * 2^length rest + fromBin rest

{- 
   Opgave 4:
   toBin Int->[Int]
   Breek een binair getal op in de indiviuele getallen 
-}
toBin :: Int->[Int]
toBin 0 = []
toBin getal = toBin (getal/2) ++ [mod getal 2]

{-
   Opgave 5:
   fromBase Int->[Char]->Int
   Zet een getal met basis n om in een cijferreeks
-}
fromBase :: Int->[Char]->Int
fromBase _ [] = 0
fromBase n (getal:rest) | n < 2 = 0
                        | n > 36 = 0 
                        | (charToInt getal) > n = 0
                        | otherwise = charToInt getal * n^length rest + fromBase n rest

{-
   Opgave 6:
   toBase Int->Int->[Char]
   Geef de cijferreeks van een getal met grondtal n
-}
toBase :: Int->Int->[Char]
toBase _ 0 = []
toBase n getal | n < 2 = []
               | n > 36 = []
               | otherwise = map intToChar (toBase' n getal)

{-
   Opgave 7:
   numbers :: Int->[String]->[(String,Int)]
   Produceert een lijst met woorden die overeenkomen met een getal.
-}
numbers :: Int->[String]->[(String,Int)]
numbers _ [] = []
numbers n (first:rest) | (fromBase n first) > 0 = (first,(fromBase n first)) : numbers n rest
                       | otherwise = numbers n rest

{-
   Opgave 10:
   keithGetallen :: [Int]
   Construeer een (oneindige) lijst die alle Keith-getallen in oplopende volgorde bevat
-}

keithGetallen :: [Int]
keithGetallen = filter isKeithGetal (vanaf 1)