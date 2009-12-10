{- Getallen.hs v1.04 (bèta)         -
 - created by:  Meindert-Jan Kroese -
 - login:       mkroese             -
 - stud.nr:     3291685             -
 - using:       Notepad++           -}
module Getallen
where
{- Importing Char to be able to convert ASCII to Integers -}
import Char

{- The toSpecial function will generate for any base an array of integers -}
toSpecial :: Int -> Int -> [Int]
toSpecial base i | i < 0 = error "Please don't give me negative Int's!"
              | i < base = [i]
              | otherwise = toSpecial base ((i-r) `div` base) ++ [r]
    where r = i `rem` base

{- The fromSpecial function will generate for any base a decimal integer representation -}
fromSpecial :: Int -> [Int] -> Int
fromSpecial _ [] = 0
fromSpecial base xs 
    | or (map (\y -> y >= base || y<0) xs) 
        = error "Please provide a list only containing values in the given range"
    | otherwise = sum (zipWith (*) (reverse xs) [base^x | x <- [0..] ])
        -- Long live lazy eval

{- Convert Char will result any of the allowed Chars to an Int -}
convertCharInt :: Int -> Char -> Int
convertCharInt limit c 
    | d >= limit = error "The specified input does not comply with the given range"
    | otherwise = d
        where 
        d | isDigit c = ord c - ord '0' 
          | isLower c = ord c - (ord 'a') + 10
          | otherwise = error "Only characters allowed are 0-9 and a-z"

{- and a function to convert a number back to a Char -}
convertIntChar :: Int -> Int -> Char
convertIntChar limit n 
    | n >= limit || n < 0 || n >= 36 = rangeerr
    | n < 10 = chr (ord '0' + n)
    | otherwise = chr (ord 'a' + (n - 10))
    where
        rangeerr = error "The specified input does not comply with the given range"

{- Opgave 1 - Implementation of fromSpecial -}
fromDec :: [Int] -> Int
fromDec = fromSpecial 10

{- Opgave 2 - Implementation of toSpecial -}
toDec :: Int -> [Int] 
toDec = toSpecial 10

{- Opgave 4 - in the same way as with dec we use fromspecial -}
toBin :: Int -> [Int]
toBin = toSpecial 2

{- Opgave 3 - ... and toSpecial -}
fromBin :: [Int] -> Int
fromBin = fromSpecial 2

{- Opgave 5 - using map to convert the String to a list of Int's -}
fromBase :: Int -> String -> Int
fromBase x 
    | x<2 || x>36 = error "Invalid base"
    | otherwise = fromSpecial x . map (convertCharInt x)

{- Opgave 6 - and the other way round -}
toBase :: Int -> Int -> String
toBase x 
    | x<2 || x>36 = error "Invalid base"
    | otherwise = map (convertIntChar x) . toSpecial x
    
{- Opgave 7 - a way to make tuples of the specified strings and the number with a given base -}
numbers :: Int -> [String] -> [(String, Int)]
numbers x css = zip css' (map (fromBase x) css')
    where
    css' = filter hasBase css
    
    hasBase :: String -> Bool
    hasBase cs = and (map hasBase' cs)
        where
        hasBase' :: Char -> Bool
        hasBase' c | isLower c = (ord c - ord 'a' + 10) < x
                   | isDigit c = (ord c - ord '0') < x
				   
{- Opgave 8 - Gray coding -}
grayCode :: Int -> ([Char] -> Int, Int -> [Char])
grayCode base = (fromGray, toGray)
	where
	fromGray :: [Char] -> Int
	fromGray = fromGray' . map (convertCharInt base) -- Lijst met Ints
		where
		fromGray' _ = undefined
	toGray :: Int -> [Char]
	toGray = map (convertIntChar base) . (toGray' 0) . (toSpecial base)
		where
		toGray' _ [] = []
		toGray' shift (x:xs) = v : (toGray' (v+shift) xs)
			where v = (x - shift) `mod` base

{- Opgave 9 - Look and Say reeksen -}
lookAndSay :: Int -> [String]
lookAndSay start = startstr : (lookAndSay' (startstr))
    where
    startstr = show start
    lookAndSay' p = las:(lookAndSay' las)
        where
        las = (lookAndSay'' p)
        
        lookAndSay'' [] = []
        lookAndSay'' (c1:cs) = show (n+1) ++ (c1:(lookAndSay'' (drop n cs)))
            where
            n = (length (takeWhile (== c1) cs))

{- Opgave 10 - Keith getallen -}
keithGetallen :: [Int]
keithGetallen = filter isKeith [10..]
    where
    isKeith x = (last ys) == x 
    -- numbers in the base can't be a Keith number because they must be <= 9
        where
        base = toDec x
        ys = takeWhile (<= x) (next base)
            where
            next xs = (\v -> v:(next (drop 1 xs++[v]))) (sum xs)
            -- next xs = (\v -> v:(next (reverse (v:(reverse (drop 1 xs)))))) (sum xs)
            -- initial code is slower (double reverse is slower than one concat, at larger numbers)

-- Hope you had fun reading this piece of code, as a final statement I would like to say: --
            
{-
##     ##    ###     ######  ##    ## ######## ##       ##       
##     ##   ## ##   ##    ## ##   ##  ##       ##       ##       
##     ##  ##   ##  ##       ##  ##   ##       ##       ##       
######### ##     ##  ######  #####    ######   ##       ##       
##     ## #########       ## ##  ##   ##       ##       ##       
##     ## ##     ## ##    ## ##   ##  ##       ##       ##       
##     ## ##     ##  ######  ##    ## ######## ######## ######## 

                        ##     ##    ###    ##     ## ##     ## 
                        ##     ##   ## ##    ##   ##   ##   ##  
  #######     #######   ##     ##  ##   ##    ## ##     ## ##   
                        ######### ##     ##    ###       ###    
  #######     #######   ##     ## #########   ## ##     ## ##   
                        ##     ## ##     ##  ##   ##   ##   ##  
                        ##     ## ##     ## ##     ## ##     ## 
-}

--                                    ...Considering its IO and UIs                      --
