--vraag 1--
fromDec:: [Int] -> Int
fromDec [] = 0
fromDec (x:xs) = x * 10 ^ length xs + fromDec xs

--vraag 2--
toDec :: Int -> [Int]
toDec 0 = []
toDec x = toDec (div x 10) ++ [rem x 10]

--vraag 3--
fromBin :: [Int] -> Int
fromBin [] = 0
fromBin (x:xs) = x * 2 ^ length xs + fromBin xs

--vraag 4--
toBin :: Int -> [Int]
toBin 0 = []
toBin x = toBin (div x 2) ++ [rem x 2]

--vraag 5--
fromBase :: Int -> [Char] -> Int
fromBase _ []     = 0
fromBase 0 _      = error "no base with 0"
fromBase b (c:cs) = if  int /= -1
                    then int * b ^ length cs + fromBase b cs
                    else error ([c] ++ " not a digit or character")
                    where int = toInt c

--vraag 6--
toBase :: Int -> Int -> [Char]
toBase _ 0 = []
toBase b n = if b > 1 && b <= 36
             then toBase b (div n b) ++ [toChar (rem n b)]
             else error "Base not correct"

toInt :: Char -> Int
toInt c = if n >= 97 && n <= 122 then n - 87
          else if n >= 48  && n <= 57 then n-48
               else -1
          where n = primOrd c

toChar :: Int -> Char
toChar n = if n > 9 then primChr (87+n)
           else primChr (48 + n)

test :: Int -> String -> Bool
test _ []     = True
test n (x:xs) = if toInt x < n
                then True && test n xs
                else False
--vraag 7--
numbers :: Int -> [String] -> [(String,Int)]
numbers _ []     = []
numbers b (x:xs) = if test b x then (x,fromBase b x) : numbers b xs
                   else numbers b xs

--vraag 8--
greyCode :: Int -> ([Char] -> Int,Int -> [Char])
greyCode x | x < 2  = error "base too low"
           | x > 36 = error "base too high"
           | otherwise = (fromBase x,toBase x)

--vraag 9
lookAndSay :: Int -> [String]
lookAndSay x = (show x) : lookAndSay (readInt (step x))

step :: Int -> [Char]
step 0 = []
step x = show (counted) ++ (take 1 (showed)) ++ step(readInt (drop (counted) (showed)))
         where counted = count x
               showed = show x

count :: Int -> Int
count x = length( takeWhile (==digit) string )
          where digit = head string
                string = show x

--vraag 10
keith :: [Int]
keith = concat (map tryKeith [10..])

--Int moet groter zijn dan 9 ivm meerdere getallen voor Keith, test weggelaten voor snelheid.
tryKeith :: Int -> [Int]
tryKeith x | head (drop ((length (toDec x)) - 1) (until grge summen (toDec x ))) == x = x:[]
           | otherwise = []
             where summen z = (drop 1 z) ++ (sum z : [])
                   grge z = sum z > x