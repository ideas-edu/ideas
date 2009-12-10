module Main where
import Char


-- Bart Spaans, FP practicum I: Getallen.
-- GHCi version 6.8.2 (op 64 bit Linux)


----------------------
-- GETALLENSYSTEMEN --
----------------------



fromBaseInt :: Int -> [Int] -> Int
toBaseInt :: Int -> Int -> [Int]
fromDec :: [Int] -> Int
fromBin :: [Int] -> Int
toDec :: Int -> [Int]
toBin :: Int -> [Int]
fromDigit :: Char -> Int
fromBase :: Int -> [Char] -> Int
toDigit :: Int -> Char
toBase :: Int -> Int -> [Char]
numbers :: Int -> [String] -> [(String, Int)]


-- fromBaseInt: geschreven voor opgave 1 en 3; wordt ook gebruikt in 
-- fromBase (opgave 5)
--
-- bMachten produceert een lijst [ b ** n - 1, b ** n -2 .. 1 ] waar n de 
-- lengte van de opgegeven lijst xs is. Bijv. voor [3, 2, 1] in base 10 zou deze 
-- functie [100, 10, 1] teruggeven. Dit resultaat wordt gezipt met xs 
-- (wat in het voorbeeld [(100, 3), (10, 2), (1, 1)] zou zijn), waarna de 
-- tuples onderling worden vermenigvuldigd (nu: [300, 20, 1]), 
-- opdat de overgebleven lijst gesommeerd kan worden.

fromBaseInt base xs = sum $ zipWith (*) bMachten xs 
  where bMachten = scanr (*) 1 $ take (length xs - 1) $ repeat base



-- toBaseInt: geschreven voor opgave 2 en 4; wordt ook gebruikt in toBase (opgave 6)
--
-- convert bouwt de lijst omgekeerd op dmv simpele mod en div functies.
-- De recursie ontstaat door n telkens door base te delen en het overblijfsel
-- te consen, totdat n 0 is.

toBaseInt base n = reverse $ convert n
   where convert 0 = []
         convert n = mod n base : (convert $ div n base)


fromDec = fromBaseInt 10 -- Opgave 1
toDec = toBaseInt 10     -- Opgave 2
fromBin = fromBaseInt 2  -- Opgave 3
toBin = toBaseInt 2      -- Opgave 4



-- Opgave 5
--
-- Deze opgave was door de eerder geschreven functie snel te doen.
-- fromDigit is nodig om de [Char] naar [Int] om te kunnen zetten, zodat 
-- de lijst door fromBaseInt gebruikt kan worden. De functie wordt ook
-- vanaf opgave 7 nog een aantal keer gebruikt.
-- Er wordt aangenomen dat alleen valide characters worden aangeleverd.

fromDigit c | isDigit c = digitToInt c
            | isLower c = ord c - ord 'a' + 10

fromBase b xs = fromBaseInt b $ map fromDigit xs


-- Opgave 6
--
-- Ook toBase is nu makkelijk en heeft alleen toDigit nodig om de lijst om te zetten.
-- toDigit wordt ook gebruikt in opgave 8 en 9.

toDigit i | i < 10= intToDigit i
          | otherwise = chr $ ord 'a' + i - 10

toBase b n = map toDigit $ toBaseInt b n


-- Opgave 7
--
-- List comprehension. Converteer elke String in de lijst naar [Int] met fromDigit
-- en kijk of elk van die Ints kleiner is dan de base b. Alleen als dat het geval is 
-- wordt de String toegevoegd aan het eindresultaat en omgezet met fromBase.

numbers b xs = [ ( x, fromBase b x ) | x <- xs, and (map (\j -> fromDigit j < b) x) ]




---------------------
-- GRAY CODERINGEN --
---------------------




grayCode :: Int -> ([Char] -> Int, Int -> [Char])
fromGrayCode :: Int -> [Char] -> Int
toGrayCode :: Int -> Int -> [Char]


-- Opgave 8

grayCode b = ( fromGrayCode b, toGrayCode b)

-- Om het overeenkomstige natuurlijk getal van een gray coded string te 
-- krijgen moet de digit voorafgaand aan n bij n opgeteld worden, modulo base, 
-- waar n de plaats in de string is.
-- fromGrayCode 2 "110" => fromBase 2 "100" => 4
--
-- convert verwacht een lijst en een nummer om te onthouden. Dit nummer wordt 
-- toegevoegd aan het volgende nummer in de lijst, modulo base. 
-- In toGray's geval wordt het volgende nummer juist verminderd met het 
-- onthouden nummer. Ook de recursieve aanroep naar convert in toGray is iets anders,
-- maar voor de rest zijn de functies identiek. Daardoor kan een meer abstracte functie
-- grayCodeConvert gebruikt worden; die twee functies als extra argumenten verwacht.

grayCodeConvert b xs add oper = convert xs 0
   where convert [] _ = []
         convert (x:xs) i = toDigit added : convert xs (add i + added)
             where added = mod (fromDigit x `oper` i) b


fromGrayCode b xs = fromBase b $ grayCodeConvert b xs (\x -> 0) (+)
toGrayCode b n = grayCodeConvert b (toBase b n) id (-)



--------------------------
-- LOCK AND SAY REEKSEN --
--------------------------



lookAndSay :: Int -> [String]

-- Opgave 9
--
-- Zet i zelf vooraan de lijst LookAndSayFunc van i
lookAndSay i = toBase 10 i : lookAndSayFunc i


-- look is een tel functie die door lookAndSayFunc wordt aangeroepen. c onthoudt het 
-- aantal opeenvolgende `last` characters. Als `last` niet hetzelfde is als de Char 
-- die bekeken wordt, dan worden c en `last` aan het eindresultaat toegevoegd 
-- (mits c niet 0 is). 

lookAndSayFunc i = l : (lookAndSayFunc (fromBase 10 l))
   where l = (look (map fromDigit (toBase 10 i)) 0 0)
         look [] c last = result c last
         look (x:xs) c last | x == last = look xs (c + 1) x
                            | otherwise = (result c last) ++ look xs 1 x
         result c n | c == 0 = ""
                    | otherwise = toDigit c : toDigit n : []




--------------------
-- KEITH GETALLEN --
--------------------



keithGetallen :: [Int]
keithReeks :: Int -> Int -> Int



-- Opgave 10
--
-- Een filter op een oneindige lijst. KeithGetal zelf maakt ook gebruik van lazy 
-- evaluation om de oneindige reeks van i te genereren met items groter 
-- of gelijk aan i. Vervolgens wordt getest of het eerste item in die 
-- lijst daadwerkelijk gelijk is aan i; alleen als dat zo is dan is i een keithGetal.

keithGetallen = filter (keithGetal) [10 .. ]
   where keithGetal i = i == ( head $ [ x | x <- map (keithReeks i) [1 .. ], x >= i ] )


-- Simpele, recursieve functie. Levert het j-de element in de reeks van i.

keithReeks i j   | j <= n = digits !! (j - 1)
                 | otherwise = sum [ keithReeks i x | x <- [j - n .. j - 1] ]
                 where digits = map fromDigit (toBase 10 i)
                       n = length digits




-- Ok, ik ben overtuigd :) 
-- Haskell rocks!



-----------
-- TESTS --
-----------

testFromDec        =  and [ fromDec [4, 2, 3] == 423, fromDec [ 4, 0, 3] == 403 ]
testToDec          =  toDec 423 == [4, 2, 3] 
testFromBin        =  fromBin [1, 0, 0, 1, 1, 0 ] == 38
testToBin          =  toBin 38 == [1, 0, 0, 1, 1, 0 ]

testFromBase       =  and [ fromBase 10 "423" == 423, fromBase 2 "100110" == 38,
                            fromBase 16 "f4c8" == 62664, fromBase 23 "5j" == 134 ]
testToBase         =  and [ toBase 10 423 == "423", toBase 2 38 == "100110",
                            toBase 16 62664 == "f4c8", toBase 23 134 == "5j" ]

testNumbers        =  numbers 16 ["ace", "face", "faces"] == [("ace", 2766), ("face", 64206)]

testGrayCode       =  and $ map ( == [1..150] ) (map (\x -> map (fst (grayCode x)) $ 
                              map (snd (grayCode x)) [1..150]) [2..30])

testLookAndSay     =  and [ take 6 (lookAndSay 1) == ["1", "11", "21", "1211", "111221", "312211"],
                            take 6 (lookAndSay 22) == ["22", "22", "22", "22", "22", "22"] ]
testKeithGetallen  =  take 5 keithGetallen == [14, 19, 28, 47, 61]

-- sanity check
testFunc f = do
                 putStr "Testing "
                 putStr $ snd f
                 putStr "..."
                 if fst f
                    then putStrLn "OK"
                    else putStrLn "ERR"

main = foldr (>>) (return ()) $ map testFunc 
                      [ (testFromDec, "conversion of integer list to integer"), 
                        (testToDec, "conversion of integer to list of integers"),
                        (testFromBin, "conversion of binary integer list to integer"),
                        (testToBin, "conversion of integer list to binary"),
                        (testFromBase, "conversion of integer string to integer in base 2-36"), 
                        (testToBase, "conversion of integer to integer string in base 2-36"),
                        (testNumbers, "number words function"),
			(testGrayCode, "gray code conversion"),
			(testLookAndSay, "Conway's look and say function"),
			(testKeithGetallen, "Keith numbers") ]

