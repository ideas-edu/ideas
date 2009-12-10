module Practicum1 where
-----------------------------
-- Steven van Driel - 3245535
-- Helium 1.6 onder Windows
-----------------------------

-- fromDec: Given a list of integers, will form these into a real number, based on order.
fromDec:: [Int]->Int
fromDec [] = 0
fromDec (x:xs) = (x * 10 ^ length (xs))+ fromDec (xs)

-- toDec: Given an integer will form a list of integers, sorted form the highest to lowest power of 10.
-- -> in the length counter (n) I used + 0.1 to offset a slight computational error, because 
--    floor ((log (fromInt 1000000000 + 0.0)) / (log 10.0)) = 5 instead of 6, as it is supposed to be
toDec:: Int->[Int]
toDec x |x > 0  = toDecimal x n
        |x == 0 = [0]
        |otherwise  = toDec (abs(x))
        where n = floor ((log (fromInt x + 0.1)) / (log 10.0))

-- toDecimal: Helper function for toDec, necessary to do the actual calculations, as an extra parameter,
--            to describe the largest power of 10 smaller than the integer, is necessary
toDecimal:: Int -> Int -> [Int]
toDecimal x n | n>0  = x `div` (10 ^ n) : toDecimal (x - ((x `div` (10 ^ n))* (10^n))) (n-1)
              | n==0 = [x `div` (10 ^ n)]
              | otherwise  = error "negative exponent"

-- fromBin: Given a list of binary numbers, will form these into a real number, based on order.
fromBin:: [Int]->Int
fromBin [] = 0
fromBin (x:xs) = (x * 2 ^ length (xs))+ fromBin (xs)

-- toBin: Given an integer, will form a list of binary numbers, sorted form the highest to lowest power of 2. 
toBin:: Int->[Int]
toBin x |x > 0  = toBinominal x n
        |x == 0 = [0]
        |otherwise  = toBin (abs(x))
        where n = floor ((log (fromInt x)) / (log 2.0))

-- toBinominal: Helper function for toBin, necessary to do the actual calculations, as an extra parameter,
--              to describe the largest power of 2 smaller than the integer, is necessary
toBinominal:: Int -> Int -> [Int]
toBinominal x n | n>0  = x `div` (2 ^ n) : toBinominal (x - ((x `div` (2 ^ n))* (2^n))) (n-1)
                | n==0 = [x `div` (2 ^ n)]
                | otherwise  = error "negative exponent"

-- fromBase: converts a string of character to a real number, based on an integer (needed as well) which describes the base
--           (amount of numbers/characters) of the counting system
fromBase:: Int -> [Char] -> Int
fromBase _ [] = 0
fromBase n (x:xs) |  and (map ((>) (87+n)) (map primOrd (map toLower (x:xs) ))) = 
                        if primOrd (toLower x) >47 && primOrd (toLower x) <58  
                        then (primOrd x - 48) * n ^ length (xs) + fromBase n xs
                        else  if primOrd (toLower x) >96 && primOrd (toLower x) <(87+n) 
                              then(primOrd (toLower x) - 87) * n ^ length (xs) + fromBase n xs
                              else (0-1)
                  | otherwise = (0-1)

-- toLower: function that converts uppercase letters to lowercase letters
toLower :: Char -> Char
toLower c
    | isUpper c = primChr ( primOrd c - primOrd 'A' + primOrd 'a' )
    | otherwise = c

-- isUpper: function that describes (by a boolean) if a character is an uppercase letter
isUpper :: Char -> Bool
isUpper c = primOrd c >= primOrd 'A' && primOrd c <= primOrd 'Z' 

-- toBase: given two integers, will convert the second integer into a list of characters, which describes the second integer
--         in a counting system with a base equal to the first integer
toBase:: Int -> Int -> [Char]
toBase n x |x > 0  = toBasic n x k
           |x == 0 = ['0']
           |otherwise  = toBase n (abs(x))
           where k = floor ((log (fromInt x + 0.1)) / (log (fromInt n+ 0.0)))

-- toBasic: Helper function for toBase, necessary to do the actual calculations, as an extra parameter,
--         to describe the largest power of n (n being the base of the counting system) smaller 
--         than the integer, is necessary
toBasic:: Int -> Int -> Int -> [Char]
toBasic n x k | k>0  = if (x `div` (n ^ k)) > 9 
                       then primChr ((x `div` (n ^ k)) +87) : toBasic n (x - ((x `div` (n ^ k))* (n^k))) (k-1)
                       else primChr ((x `div` (n ^ k)) +48) : toBasic n (x - ((x `div` (n ^ k))* (n^k))) (k-1)
              | k==0 = if (x `div` (n ^ k)) > 9 
                       then [primChr ((x `div` (n ^ k)) +87)] 
                       else [primChr ((x `div` (n ^ k)) +48)]
              | otherwise  = error "negative exponent"

-- numbers: given a list of words and an integer, will return those words which represent a number in the
--          counting system with a base equal to the integer, and the number that they represent
numbers:: Int -> [String] -> [(String, Int)]
numbers _ [] = []
numbers n (x:xs) | (fromBase n x) >= 0 = (x, fromBase n x) : numbers n xs
                 | otherwise         = numbers n xs

-- grayCode: returns to functions, based on an integer. the function are to convert from and to graycode, based on a counting system
--           with a base equal to the integer
graycode:: Int -> ([Char] -> Int, Int -> [Char])
graycode b = ((graycodeTerug b), (graycodeHeen b))

-- listDivide: divides a list into a list of lists, with each of the contained lists containing one character from the original list
listDivide:: [Char] -> [[Char]]
listDivide [] = [[]]
listDivide (x:xs) | length(xs)==0  = [[x]]
                  | otherwise      = [x]:listDivide(xs)

-- graycodeHeen: function that returns a string of characters which describe the second integer as a gray code in the counting system
--               with a base equal to the first integer
graycodeHeen:: Int -> Int -> [Char]
graycodeHeen b x = concat (map (toBase b) (graycodeHeenX (0) (b) (map (fromBase b) (listDivide (toBase b x)))))

-- graycodeHeenX: helper function of graycodeHeen, makes use of a list of integers as given by graycodeHeen.
--                necessary because of the iteration component involved in the calculations
graycodeHeenX:: Int -> Int -> [Int] -> [Int]
graycodeHeenX _ _ [] = []
graycodeHeenX n b (x:xs) = a:graycodeHeenX (n+a) b (xs)
              where a = ((x+b-n)`mod`b)

-- graycodeTerug: function that returns an integer which is the real number from which the graycode given as a parameter was produced
--                by the above algorithm. as a graycode is not unique, it is not possible to inverse the graycodeHeen function.
graycodeTerug:: Int -> [Char] -> Int
graycodeTerug _ [] = 0
graycodeTerug b (x:xs) = graycodeTerugX ((fromBase b [x])*(b^k)) b (x:xs)
                      where k = length (xs)

--floor ((log (fromInt (fromBase b (x:xs)) + 0.1)) / (log (fromInt b + 0.0)))

-- graycodeTerugX: tries every graycode within the range of the given graycode (based on length and first character of graycode)
--                 and compares these with the given graycode
graycodeTerugX:: Int -> Int -> [Char] -> Int
graycodeTerugX n b (x) | (x) == graycodeHeen b n = n
                       | otherwise               = graycodeTerugX (n+1) b (x)

-- lookAndSay: produces, based on an integer, the infinite look-and-say sequence based on this integer
lookAndSay:: Int -> [String]
lookAndSay n = concat[map primChr (map ((+) 48)(toDec n))]: lasStand (toDec n)
   
-- lasStand: helper function for lookAndSay, converts the Int result from lasHelp into a String result
lasStand:: [Int] -> [String]
lasStand (n) = concat[map primChr (map ((+) 48)(lasHelp n))] : (lasStand (lasHelp n))

-- lasHelp: helper function for lookAndSay, adds together the number sequences provided by lasIt
lasHelp::[Int] -> [Int]
lasHelp []  = []
lasHelp (x) = (take 2 (lasIt (x) (1))) ++ (lasHelp(drop 2 (lasIt (x)(1))))

-- lasIt: helper function for lookAndSay, counts the number of equal numbers at the start of the string and returns the amount, one of those 
--        numbers and the rest of the string
lasIt:: [Int] -> Int -> [Int]
lasIt [] _ = []
lasIt (x:xs) n | xs==[]    = (n:x:xs)
               | otherwise = if   x==head(xs)
                             then lasIt (xs) (n+1)
                             else (n:x:xs)

--keithGetallen: returns all Keith numbers, that is, all integers which are contained within the Keith sequence based on itself
keithGetallen:: [Int]
keithGetallen = filter (keithReeks) [10..]

-- keithReeks: returns if an integer is a Keith number
keithReeks:: Int -> Bool
keithReeks a = (last(until (keithBool (a)) (keithBerekening (length (toDec a))) (toDec a))) == a

-- keithBool: returns of the last number of a list of integers is greater than or equal to the integer given to this function
keithBool:: Int -> [Int] -> Bool
keithBool n (x) = (last x)>=n

-- keithBerekening: returns a list with the next number in a Keith sequence at the end of a string which contains the tail of the 
--                  given string (which are the numbers in the Keith sequence before itself), the extra Int tells how long the original
--                  Keith number was
keithBerekening:: Int -> [Int] -> [Int]
keithBerekening _ [] = []
keithBerekening n (x:xs) = ((xs) ++ [sum(take n(reverse(x:xs)))]) 
