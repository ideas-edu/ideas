-- Team: Yorick Bouma 3407020
-- Compiler: Helium

-- This function returns the corresponding int of a decimal sequence
-- Example: fromDec [4, 2, 3] returns 423
fromDec         :: [Int] -> Int
fromDec []      = 0
fromDec (x:y)   = x * 10 ^ length y + fromDec y

-- This function returns the corresponding decimal sequence of an int
-- Example: toDec 423 returns [4, 2, 3]
toDec   :: Int -> [Int]
toDec a | a < 10 = [a]
        | otherwise = toDec (a `div` 10) ++ [a `mod` 10]

-- This function returns the value in the decimal system of an binary value
-- Example: fromBin [1, 0, 0, 1, 1, 0] returns 38
fromBin         :: [Int] -> Int
fromBin []      = 0
fromBin (x:y)   = x * 2 ^ length y + fromBin y

-- This function returns the binary value of a given int
-- Example: toBin 38 returns [1, 0, 0, 1, 1, 0]
toBin   :: Int -> [Int]
toBin a | a < 2 = [a]
        | otherwise = toBin (a `div` 2) ++ [a `mod` 2]

-- This function returns the corresponding value of a seqeunce with a given base number
-- Example:  fromBase 16 "f4c8" returns 62664
fromBase            :: Int -> [Char] -> Int
fromBase _ []       = 0
fromBase a (x:y)    = ((fromEnum x) - b) * a ^ length y + fromBase a y
                    where b = if fromEnum x <= 57 then 48 else 87

-- This function returns the corresponding seqeunce of a value with a given base number
-- Example: toBase 16 62664 returns "f4c8"
toBase      :: Int -> Int -> [Char]
toBase a b  | b < a = [toEnum(b+c)]
            | otherwise = toBase a (b `div` a) ++ [toEnum((b `mod` a) + c)]
            where c = if b `mod` a <= 9 then 48 else 87

numbers :: Int -> [String] -> [(String, Int)]
numbers _ [] = 0;