-- Eerste practicumopgave Functioneel Programmeren 02-032009
-- Door: Jesse Feddema (3367339)

-- Algemene hulpfuncties
charToInt :: Char -> Int -- Ondersteunt kleine letters voor 10-35
charToInt c = primOrd c - (if localIsDigit c then primOrd '0' else primOrd 'a'-10)

intToChar :: Int -> Char -- Ondersteunt alle getallen 0-35
intToChar i | i < 10 = primChr (primOrd '0' + i)
            | otherwise = primChr (primOrd 'a' + i - 10)

-- Opgave 1
fromDec :: [Int] -> Int
fromDec = foldl (\x y -> 10*x+y) 0

-- Opgave 2
toDec :: Int -> [Int]
toDec y = toDec' y []
        where toDec' x result | xdiv == 0 = if (x==0) then [0] else (xmod:result)
                              | otherwise = toDec' xdiv (xmod:result)
                              where xdiv = x `div` 10
                                    xmod = x `mod` 10

-- Opgave 3
fromBin :: [Int] -> Int
fromBin = foldl (\x y -> 2*x+y) 0

-- Opgave 4
toBin :: Int -> [Int]
toBin y = toBin' y []
        where toBin' x result | xdiv == 0 = if (x==0) then [0] else (xmod:result)
                              | otherwise = toBin' xdiv (xmod:result)
                              where xdiv = x `div` 2
                                    xmod = x `mod` 2
-- Opgave 5
fromBase :: Int -> [Char] -> Int
fromBase g = foldl (\x y -> g*x+charToInt y) 0

-- Opgave 6
toBase :: Int -> Int -> [Char]
toBase g y = toBase' y []
           where toBase' x result | xdiv == 0 = if (x==0) then ['0'] else (xmod:result)
                                  | otherwise = toBase' xdiv (xmod:result)
                                  where xdiv = x `div` g
                                        xmod = intToChar (x `mod` g)

-- Opgave 7
numbers :: Int -> [String] -> [(String, Int)]
numbers g = foldr (\x y -> if (validnumber x) then (x,fromBase g x):y else y) []
          where validnumber x = case x of
                                [] -> True
                                (c:cs) -> (charToInt c <= g) && validnumber cs

-- Opgave 8
grayToNat' :: Int -> String -> Int -> Int -> Int -- Maakt gebruik van het grondtal, de Gray-code, een richting (0 of 1) en de lengte van de (overgebleven) code; geeft het bij de code horende natuurlijk getal
grayToNat' _ [] _ _ = 0
grayToNat' g (c:cs) dir l = addedNumber + grayToNat' g cs (abs ((c' `mod` 2)-dir)) (l-1)
                          where c' = charToInt c
                                addedNumber = (abs ((dir*(g-1))-c'))*(g^(l-1))

grayToNat :: Int -> String -> Int
grayToNat g str = grayToNat' g str 0 (length str)

natToGray' :: Int -> String -> Int -> String -- Maakt gebruik van het grondtal, het natuurlijke getal op basis van het grondtal en een richting (0 of 1); levert de Gray-code op
natToGray' _ [] _ = []
natToGray' g (c:cs) dir = intToChar(abs ((dir*(g-1))-c')) : natToGray' g cs (abs ((c' `mod` 2)-dir))
                        where c' = charToInt c

natToGray :: Int -> Int -> String
natToGray g nat = natToGray' g (toBase g nat) 0

grayCode :: Int -> ([Char] -> Int, Int -> [Char])
grayCode g = (grayToNat g, natToGray g)

-- Opgave 9
splitToEquals :: String -> [String] -- Toelichting: maakt van "1123331" ["11","2","333","1"]
splitToEquals [] = []
splitToEquals (c':cs') = splitToEquals' (c',[c']) cs'
                     where splitToEquals' (ct,eqs) rest = case rest of
                                                          [] -> [eqs]
                                                          (c:cs) -> if c == ct then splitToEquals' (ct,c:eqs) cs else eqs:splitToEquals' (c,[c]) cs

lookAndSayString :: String -> String -- Voor het principe uit op een enkele string
lookAndSayString str = foldr (\x y -> (intToChar (length x)):(head x):y) [] (splitToEquals str)

lookAndSay :: Int -> [String]
lookAndSay g = [(intToChar g)] : lookAndSay' [(intToChar g)]
             where lookAndSay' str = (lookAndSayString str) : lookAndSay' (lookAndSayString str)

-- Opgave 10
keithReeks :: [Int] -> [Int] -- Geeft de reeks Keithgetallen voor een enkel natuurlijk getal groter dan 9, uitgedrukt zoals na toDec
keithReeks [] = []
keithReeks gs = gs ++ zipWith (+) (keithReeks gs) (keithReeks' ((length gs)-1) tail)
              where keithReeks' l tailer | l == 1 = tailer (keithReeks gs)
                                         | otherwise = zipWith (+) (tailer (keithReeks gs)) (keithReeks' (l-1) (tailer.tail))

isKeithGetal :: Int -> Bool
isKeithGetal g = last (takeWhile (<=g) (keithReeks (toDec g))) == g

keithGetallen :: [Int]
keithGetallen = filter isKeithGetal [10..]
