-------------------------------------------------------------------
--      Practicum 1 Functioneel Programmeren '09: Getallen       --
--                  Gemaakt door: Linda Stoel                    --
--                    Studentnummer: 3345653                     -- 
--                  Gebruikte compiler: Helium                   --
-------------------------------------------------------------------

-- Known bugs: De functie grayCode (opdracht 8) geeft als output een tupel met twee functies waarvan de eerste niet naar behoren werkt (de tweede werkt wel)

module Practicum1Getallen where

-- Hulpfuncties ---------------------------------------------------

-- toAlpha geeft bij een getal x uit 0..36 zijn equivalente waarde uit '0'..'9' of 'a'..'z'
-- wordt gebruikt in: toBase, grayCode (opdrachten 6 en 8)
toAlpha :: Int -> Char
toAlpha x | 0  <= x && x <= 9  = primChr (x + primOrd '0')
          | 10 <= x && x <= 36 = primChr (x - 10 + primOrd 'a')
          | otherwise          = error "De parameter van toAlpha moet een geheel getal tussen 2 en 36 (inclusief) zijn"

-- toNum geeft bij een teken x uit '0'..'9' of 'a'..'z' zijn equivalente waarde uit 0..36
-- wordt gebruikt in: inBase (hulpfunctie), fromBase, grayCode (opdrachten 5 en 8)
toNum :: Char -> Int
toNum x | primOrd '0' <= primOrd x && primOrd x <= primOrd '9' = primOrd x - primOrd '0'
        | primOrd 'a' <= primOrd x && primOrd x <= primOrd 'z' = primOrd x - primOrd 'a' + 10
        | otherwise                                            = error "De parameter van toNum moet een karakter uit '0'..'9' of 'a'..'z' zijn"

-- goodBase checkt of een getal b tussen 2 en 36 (inclusief) ligt, en dus of het een grondtal is waarop de functies in deze module goed gedefinieert zijn
-- wordt gebruikt in: fromBase, toBase, numbers, grayCode (opdrachten 5, 6, 7 en 8)
goodBase :: Int -> Bool
goodBase b | 2 <= b && b<=36 = True
           | otherwise       = False

-- inBase checkt of een tekenreeks x goed gedefinieert is binnen het talstelsel met grondtal b
-- wordt gebruikt in: fromBase, numbers (opdrachten 5 en 7)
inBase :: Int -> [Char] -> Bool
inBase b x = foldl (&&) True (map ((<b).toNum) x)

-- keithReeksUntil berekent de Keith-reeks bij het getal start totdat de waarden in de reeks groter dan of gelijk aan end zijn
-- wordt gebruikt in: isKeith (hulpfunctie)
keithReeksUntil :: Int -> Int -> [Int]
keithReeksUntil end start = until ((>= end).last) (next (length startlist)) startlist
                          where startlist :: [Int]
                                startlist = toDec start
                                next :: Int -> [Int] -> [Int]
                                next a b = b ++ [sum (drop ((length b) -a) b)]

-- isKeith checkt of een getal x een Keith-getal is
-- wordt gebruikt in: keithGetallen (opdracht 10)
isKeith :: Int -> Bool
isKeith x = x == (last (keithReeksUntil x x))


-- Opdracht 1 -----------------------------------------------------

-- fromDec zet een reeks decimale cijfers om in de bijbehorende interpretatie als som van 10-machten
fromDec :: [Int] -> Int
fromDec (x:xs) = x * 10^(length xs) + fromDec xs
fromDec []     = 0

-- Opdracht 2 -----------------------------------------------------

-- toDec zet een getal x om naar de bijbehorende decimale cijferreeks
toDec :: Int -> [Int]
toDec x | x<0       = map (*(-1)) (toDec (-x))
        | x<10      = [x]
        | otherwise = toDec (x `quot` 10) ++ [x `rem` 10]

-- Opdracht 3 -----------------------------------------------------

--fromBin zet een reeks binaire cijfers om in de bijbehorende interpretatie als som van 2-machten
fromBin :: [Int] -> Int
fromBin (x:xs) = x * 2^(length xs) + fromBin xs
fromBin []     = 0

-- Opdracht 4 -----------------------------------------------------

-- toBin zet een getal x om naar de bijbehorende binaire cijferreeks
toBin :: Int -> [Int]
toBin x | x<0       = map (*(-1)) (toBin (-x))
        | x<2       = [x]
        | otherwise = toBin (x `quot` 2) ++ [x `rem` 2]

-- Opdracht 5 -----------------------------------------------------

-- fromBase zet voor een grondtal b tussen 2 en 36 (inclusief) een cijferreeks om in de bijbehorende intepretatie als som van b-machten
fromBase :: Int -> [Char] -> Int
fromBase b (x:xs) | not (goodBase b)          = error "De eerste parameter van fromBase moet een geheel getal tussen 2 en 36 (inclusief) zijn"
                  | inBase b xs && x == '-'   = (- fromBase b xs)
                  | not (inBase b (x:xs))     = error "Het getal dat als tweede parameter van fromBase is opgegeven is niet gedefinieerd in de opgegeven basis"
                  | otherwise                 = toNum x * b^(length xs) + fromBase b xs
fromBase _ []     = 0


-- Opdracht 6 -----------------------------------------------------

-- toBase zet voor een grondtal b tussen 2 en 36 (inclusief) een getal x om naar de bijbehorende cijferreeks
toBase :: Int -> Int -> [Char]
toBase b a | not (goodBase b) = error "De eerste parameter van toBase moet een geheel getal tussen 2 en 36 (inclusief) zijn"
           | a<0              = ['-'] ++ (toBase b (-a))
           | a<b              = [toAlpha a]
           | otherwise        = toBase b (a `quot` b) ++ [toAlpha (a `rem` b)]


-- Opdracht 7 -----------------------------------------------------

-- numbers zet voor een grondtal b tussen 2 en 36 (inclusief) een lijst met woorden om in eel lijst van paren van woorden die overeenkomen met een getal en het bijbehorende getal
numbers :: Int -> [String] -> [(String,Int)]
numbers b (x:xs) | not (goodBase b) = error "De eerste parameter van numbers moet een geheel getal tussen 2 en 36 (inclusief) zijn"
                 | inBase b x       = [(x,fromBase b x)] ++ numbers b xs
                 | otherwise        = [] ++ numbers b xs
numbers _ []     = []


-- Opdracht 8 -----------------------------------------------------

-- grayCode construeert bij een  grondtal x tussen 2 en 36 (inclusief) een Gray-codering, deze codering bestaat uit een tweetal functies waarvan de eerste een cijferreeks interpreteert als een natuuurlijk getal en de tweede bij een natuurlijk getal de bijbehorende cijferreeks produceert
grayCode :: Int -> ([Char] -> Int,Int -> [Char])
grayCode x | not (goodBase x) = error "De eerste parameter van grayCode moet een geheel getal tussen 2 en 36 (inclusief) zijn"
           | otherwise = (fromGray x "ASC",toGray x)
                  where fromGray :: Int -> String -> [Char] -> Int
                        fromGray _ "ASC" (y:[])                        = toNum y
                        fromGray b "ASC" (y:ys)  | even (numy `div` b) = numy * b ^ length ys + fromGray b "ASC" ys
                                                 | otherwise           = numy * b ^ length ys + fromGray b "DESC" ys
                                                                       where numy :: Int
                                                                             numy = toNum y
                        fromGray b "DESC" (y:[])                       =  b - 1 - toNum y
                        fromGray b "DESC" (y:ys) | even (numy `div` b) = (high - numy) * b ^ length ys + fromGray b "DESC" ys
                                                 | otherwise           = (high - numy) * b ^ length ys + fromGray b "ASC" ys
                                                                       where numy :: Int
                                                                             numy = toNum y
                                                                             high :: Int
                                                                             high = b-1
                        -- toGray zet voor een grondtal b tussen 2 en 36 (inclusief) een getal a om naar de bijbehorende cijferreeks
                        toGray :: Int -> Int -> [Char]
                        toGray b a | even (a `div` b) && a<b = [toAlpha (a `mod` b)]
                                   | even (a `div` b)        = toGray b (a `div` b) ++ [toAlpha (a `mod` b)]
                                   | a<b                     = [toAlpha (b - 1 - (a `mod` b))]
                                   | otherwise               = toGray b (a `div` b) ++ [toAlpha (b - 1 - (a `mod` b))]


-- Opdracht 9 -----------------------------------------------------

-- lookAndSay produceert bij een eerste element a de oneindige look-and-say-reeks
lookAndSay :: Int -> [String]
lookAndSay a = iterate next start
             where -- start
                   start :: [Char]
                   start = toBase 10 a
                   -- next
                   next :: [Char] -> [Char]
                   next (x:xs) = toBase 10 (length (takeWhile (==x) xs) +1) ++ [x] ++ next (dropWhile (==x) xs)
                   next []     = []

-- Opdracht 10 ----------------------------------------------------

-- keithGetallen is de lijst van alle keith-getallen in oplopende volgorde
keithGetallen :: [Int]
keithGetallen = filter isKeith [10..]
