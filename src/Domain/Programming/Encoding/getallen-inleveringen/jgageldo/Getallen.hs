--Naam: Joshua van Gageldonk
--Studentnummer: 3279588
--e-mail: jos_van_gageldonk@hotmail.com / jgageldo@students.cs.uu.nl

module Getallen where

--deze test-opdracht is gemaakt om te testen of de voorbeelden uit de opdracht goed uitgevoerd worden. Als het goed is levert de test-opdracht meedere True-waardes op.
test :: IO ()
test = do
          print (fromDec [4,2,3] == 423)
          print (toDec 423 == [4,2,3])

          print (fromBin [1,0,0,1,1,0] == 38)
          print (toBin 38 == [1,0,0,1,1,0])

          print (fromBase 10 "423" == 423)
          print (fromBase 2 "100110" == 38)
          print (fromBase 16 "f4c8" == 62664)
          print (fromBase 23 "5j" == 134)
          print (toBase 10 423 == "423")
          print (toBase 2 38 == "100110")
          print (toBase 16 62664 == "f4c8")
          print (toBase 23 134 == "5j")

          print (numbers 16 ["ace", "face", "faces"] == [("ace", 2766), ("face", 64206)])

          print (take 6 (lookAndSay 1) == ["1","11","21","1211","111221","312211"])

          print (take 5 keithGetallen == [14, 19, 28, 47, 61])



{-
Om de descriptions bij de hand te hebben:

foldl op r [a, b, c] -> ((r `op` a) `op` b) `op` c
foldr op r [a, b, c] -> a `op` (b `op` (c `op` r))
-}


-----
-- intValue,
-- opsomming voor de cijfer-chars, recursie voor de letter-chars.
-- Waarschijnlijk kunnen ook de cijfer-chars met een vergelijkbare recursie.
-----
intValue :: Char -> Int
intValue '0' = 0
intValue '1' = 1
intValue '2' = 2
intValue '3' = 3
intValue '4' = 4
intValue '5' = 5
intValue '6' = 6
intValue '7' = 7
intValue '8' = 8
intValue '9' = 9

intValue 'a' = 10
intValue c | c > 'a' && c <= 'z' --isLower
            = succ (intValue (pred c))
           | otherwise
            = undefined        

-- intValue _ = undefined


-----
-- charValue,
-- opsomming voor de cijfer-ints, recursie voor de letter-ints.
-- Waarschijnlijk kunnen ook de cijfer-ints met een vergelijkbare recursie.
-----
charValue :: Int -> Char
charValue 0 = '0'
charValue 1 = '1'
charValue 2 = '2'
charValue 3 = '3'
charValue 4 = '4'
charValue 5 = '5'
charValue 6 = '6'
charValue 7 = '7'
charValue 8 = '8'
charValue 9 = '9'

charValue 10 = 'a'
charValue i | i > 10 && i <= 35
             = succ (charValue (pred i))
            | otherwise
             = undefined        

-- charValue _ = undefined


{-
fromDec en fromBin
------------------

Er zijn (minstens) twee mogelijkheden.

fromDec/Bin gebruiken een hulpfunctie waaraan de reverse van de decimale/binaire
reeks cijfers wordt doorgegeven.
Er wordt een teller (n) bijgehouden die begint bij 0 en bij elke volgende aanroep
met 1 wordt verhoogd.
De kop van de cijferreeks wordt vermenigvuldigd met 10 (decimaal) of 2 (binair)
tot de macht n en dit wordt steeds opgeteld bij de volgede recursieve aanroepen.
Grensgeval is een willekeurige teller en een lege lijst.

fromDec'/Bin' gebruiken de functie foldl met beginwaarde 0 en
operatie ((+) . (*10)).

Zelfs fromBase en fromBase' kunnen volgens deze beide mogelijkheden worden uitgewerkt -
maar dan met een variabel grondtal en een String/lijst van Chars i.p.v. een lijst
van Ints.

Opmerking: enkel de functies zonder ' testen op kleiner dan grondtal,
dus fromDec [11 (of hoger)] geeft fout, fromDec' [11] geeft 11
en fromBin [2 (of hoger)] geeft fout, fromBin' [2] geeft 2


toDec en toBin
--------------

Ook toDec/toBin gebruiken een hulpfunctie om het resultaat daarvan tenslotte
om te draaien.
De hulpfunctie zet de mod van 10 (decimaal) of 2 (binair) van het getal
op kop van de resultaatlijst en gaat recursief verder met de div van het getal.

toDec' - voor decimaal is deze variant mogelijk.

toBase - deze gebruikt echter weer de mod/div-methode. 
-}


-----
-- fromDec, toDec
-----
-----
-- fromDec
-----
fromDec :: [Int] -> Int
-- fromDec [4,2,3] == 432
fromDec ints = fromDecHulp 0 (reverse ints)
  where fromDecHulp :: Int -> [Int] -> Int
        fromDecHulp _ [] = 0
        fromDecHulp n (i:is) | i < 10    = (i*10^n) + fromDecHulp (n+1) is
                             | otherwise = undefined
        
fromDec' :: [Int] -> Int
fromDec' ints = foldl ((+) . (*10)) 0 ints -- moet foldl zijn!

-----
-- toDec
-----
toDec :: Int -> [Int]
-- toDec 423 == [4,2,3]
toDec 0   = [0]
toDec int = reverse (toDecHulp int)
  where toDecHulp :: Int -> [Int]
        toDecHulp 0 = []
        toDecHulp i = (i `mod` 10):toDecHulp (i `div` 10)

toDec' :: Int -> [Int]
toDec' int = toDecHulp' (showInt int)
  where toDecHulp' :: [Char] -> [Int]
        toDecHulp' [] = [] 
        toDecHulp' (i:is) = (intValue i):toDecHulp' is


-----
-- fromBin, toBin
-----
-----
-- fromBin
-----
fromBin :: [Int] -> Int
-- fromBin [1,0,0,1,1,0] == 38 
fromBin ints = fromBinHulp 0 (reverse ints)
  where fromBinHulp :: Int -> [Int] -> Int
        fromBinHulp _ [] = 0
        fromBinHulp n (i:is) | i < 2     = (i*2^n) + fromBinHulp (n+1) is
                             | otherwise = undefined

fromBin' :: [Int] -> Int
fromBin' ints = foldl ((+) . (*2)) 0 ints -- moet foldl zijn!

-----
-- toBin
-----
toBin :: Int -> [Int]
-- toBin 38 == [1,0,0,1,1,0]
toBin 0   = [0]
toBin int = reverse (toBinHulp int)
  where toBinHulp :: Int -> [Int]
        toBinHulp 0 = []
        toBinHulp i = (i `mod` 2):toBinHulp (i `div` 2)


-----
-- fromBase, toBase
-----
-----
-- fromBase
-----
fromBase :: Int -> [Char] -> Int
-- fromBase 10 "423" == 423 
-- fromBase 2 "100110" == 38 
-- fromBase 16 "f4c8" == 62664423 
-- fromBase 23 "5j" == 134 
fromBase grondtal cijferreeks = fromBaseHulp 0 grondtal (reverse cijferreeks)
  where fromBaseHulp :: Int -> Int -> [Char] -> Int
        fromBaseHulp _ _ [] = 0
--        fromBaseHulp n gt (cijfer:cijfers)
--          = ((intValue cijfer)*gt^n) + fromBaseHulp (n+1) gt cijfers
        fromBaseHulp n gt (cijfer:cijfers) 
          | intValue cijfer < gt = ((intValue cijfer)*gt^n) + fromBaseHulp (n+1) gt cijfers
          | otherwise            = undefined

        
fromBase' :: Int -> [Char] -> Int
fromBase' grondtal cijferreeks = foldl ((+) . (*grondtal)) 0 (map intValue cijferreeks) -- moet foldl zijn!          

-----
-- toBase
-----
toBase :: Int -> Int -> [Char]
-- toBase 10 423 == "423"
-- toBase 2 38 == "100110"
-- toBase 16 62664 == "f4c8"
-- toBase 23 134 == "5j"
toBase _ 0 = ['0']
toBase grondtal getal = map charValue (reverse (toBaseHulp grondtal getal))
  where toBaseHulp :: Int -> Int -> [Int]
        toBaseHulp _  0 = []
        toBaseHulp gt g = (g `mod` gt):toBaseHulp gt (g `div` gt)


-----
-- numbers
-----
numbers :: Int -> [String] -> [(String, Int)]
-- numbers 16 ["ace", "face", "faces"] == [("ace", 2766), ("face", 64206)]
numbers _ [] = []  
numbers grondtal (woord:woorden)
  -- Alle karakters waaruit het woord bestaat moeten kleiner zijn dan de character-waarde van het grondtal,
  -- anders niet opnemen. Filter op >= moet dus leeg zijn.
  -- Voorbeeld: grondtal 16, charValue 16 = 'g', 
  -- filter "face" = [] dus leeg, filter "faces" = "s" ofwel ['s'] dus niet leeg
  | filter (>= charValue grondtal) woord == [] = (woord, fromBase grondtal woord):numbers grondtal woorden
  | otherwise                                  =                                  numbers grondtal woorden


-----
-- lookAndSay
-----
lookAndSay :: Int -> [String]
-- take 6 (lookAndSay 1) == ["1","11","21","1211","111221","312211"]
lookAndSay element = showInt element:lookAndSay (fromBase 10 (lasVolgende (showInt element)))

-- bepaalt de volgende
lasVolgende :: String -> String
lasVolgende [] = []
lasVolgende (char:chars) = (showInt aantal) ++ [char] ++ (lasVolgende dw)
  where tw = takeWhile (== char) (char:chars)
        dw = dropWhile (== char) (char:chars)
        aantal = length tw


-----
-- keithGetallen
-----
keithGetallen :: [Int]
-- take 5 keithGetallen == [14, 19, 28, 47, 61]
keithGetallen = filter isKeithGetal (iterate (+1) 10) -- beginnen bij 10, want moet groter zijn dan negen

isKeithGetal :: Int -> Bool
isKeithGetal getal | head (dropWhile (< getal) (maakReeks getal)) == getal = True
                   | otherwise = False

maakReeks :: Int -> [Int]
maakReeks getal = beginreeks ++ maakReeksHulp beginreeks
  where beginreeks :: [Int]
        beginreeks = toDec getal

maakReeksHulp :: [Int] -> [Int]
maakReeksHulp reeks = kgVolgendeElement reeks:maakReeksHulp (kgVolgendeReeks reeks)


-- bepaalt het volgende element
kgVolgendeElement :: [Int] -> Int
kgVolgendeElement [] = 0
kgVolgendeElement reeks = foldr (+) 0 reeks -- mag ook foldl zijn

-- bepaalt de volgende reeks
kgVolgendeReeks :: [Int] -> [Int]
kgVolgendeReeks [] = []
kgVolgendeReeks (getal:getallen) = getallen ++ [kgVolgendeElement (getal:getallen)]
