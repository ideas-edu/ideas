{-
    | Prakticum 1
    | Door Peter Boot - 3344770
    | Getest met de Helium Compiler
-}


-- Opdracht 1 --
 
-- fromDec converteert een lijst representatie van een decimaal getal naar een 
--  som van 10-machten.
-- Voorbeeld: fromDec [2,8,1]  =  281
fromDec          :: [Int] -> Int
fromDec []       =  0
fromDec (x : xs) =  x * 10 ^ (length xs) + fromDec xs


-- Opdracht 2 --

-- toDec converteert een getal naar een decimale cijferreeks
-- Voorbeeld: toDec 281  =  [2,8,1]
toDec          :: Int -> [Int]
toDec n 
   | n < 10    =  [n]
   | otherwise =  toDec (n `div` 10) ++ [n `mod` 10]


--  Opdracht 3 --

-- fromBin converteert een lijst representatie van een binair getal naar een
--  decimaal getal
-- Voorbeeld: fromBin [1,1,1,0,0]  =  28
fromBin          :: [Int] -> Int
fromBin []       =  0
fromBin (x : xs) =  x * 2 ^ (length xs) + fromBin xs


-- Opdracht 4 --

-- toBin converteert een getal naar een binaire reeks
-- Voorbeeld: toBin 11  =  [1,0,1,1]
toBin   :: Int -> [Int]
toBin 0 =  []
toBin n =  toBin(n `div` 2) ++ [n `mod` 2]


-- Opdracht 5 --

-- fromBase converteert een cijferreeks in een opgegeven grondtal naar een
--  decimaal getal
-- Voorbeeld: fromBase 16 "7c5"  =  1989
fromBase            :: Int -> [Char] -> Int
fromBase _ []  =  0
fromBase b (x : xs)
   | q < b          =  q * (b ^ (length xs)) + fromBase b xs
   | otherwise      =  undefined
       where q      = base2num x

-- num2base converteert een getal naar een representatie met één teken
-- Voorbeeld: num2base 7   =  '7'
--            num2base 15  =  'f'
num2base       :: Int -> Char
num2base a
   | a <= 9    =  chr (48 + a) 
   | otherwise =  chr (87 + a)

-- base2num converteert een teken naar een decimale representatie
-- Voorbeeld: base2num 'o'  =  24
--            base2num '9'  =  9
base2num       :: Char -> Int
base2num a
   | c <=57    =  c - 48 
   | otherwise =  c - 87
       where c =  ord a

-- chr converteert een ascii-waarde naar het bijbehorende Character
-- Omdat in Helium deze functie niet bestond, hem zelf toegevoegd
-- Voorbeeld: chr 65  =  'A'
chr   :: Int -> Char
chr a =  toEnum a :: Char


-- ord converteert een Character naar de bijbehorende ascii-waarde
-- Omdat in Helium deze functie niet bestond, hem zelf toegevoegd
-- Voorbeeld: ord 'A'  =  65
ord   :: Char -> Int
ord a =  fromEnum a :: Int


-- Opdracht 6 --

-- toBase retourneert een representatie van een getal in een bepaald
--  grondtal. Het eerste argument is het grondtal, de tweede het getal
--  wat geconverteerd moet worden
-- Voorbeeld: toBase 16 255  =  "ff" 
toBase     :: Int -> Int -> [Char]
toBase _ 0 =  []
toBase b c =  toBase b (c `div` b) ++ [num2base (c `mod` b)]

-- isValidNotation controleert of een ingevoerde cijferreeks voldoet aan
--  het gespecificeerde grondtal
-- Voorbeelden: isValidNotation 2 "100101"  =  True
--              isValidNotation 2 "100201"  =  False (2 is niet binair)
isValidNotation            :: Int -> [Char] -> Bool
isValidNotation _ []       =  True
isValidNotation b (x : xs) 
   | base2num x >= b       =  False
   | otherwise             =  isValidNotation b xs


-- Opdracht 7 --

-- numbers geeft een lijst terug van ingevoerde woorden die aan het meegegeven
--  talstelsel voldoen en geeft ook de decimale waarde terug
-- Voorbeeld: numbers 16 ["ace", "face", "faces"]  =  [("ace",2766),("face",64206)]
--  ("faces" is niet geldig omdat de 's' niet in het 16tallig stelsel zit)
numbers     :: Int -> [String ] -> [(String,Int)]
numbers b s =  [(x,y) | x <- s, isValidNotation b x, y <- [(fromBase b x)]]


-- Opdracht 9 --

-- lookAndSay maakt het begin van de reeks, namelijk het ingevoerde getal en laat
--  vervolgens buildList de resterende lijst opbouwen
-- Voorbeeld: take 3 (lookAndSay 1)  =  ["1","11","21"]
lookAndSay   :: Int -> [String]
lookAndSay n =  [c] ++ buildList c
     where c =  [chr (n+48)]

-- buildList levert de lookAndSay-reeks op die gegenereerd wordt in count, behalve 
--  de startwaarde
-- Voorbeeld: take 3 (buildList ['1'])  =  ["11","21","1211"]
buildList   :: [Char] -> [String]
buildList a =  [q] ++ buildList q
               where q = count (group a) 

-- count telt het aantal dezelfde tekens achter elkaar en levert dit op, samen met het
--  teken zelf
-- Voorbeeld: count [['1','1','1'], ['2']]  =  "3112"
count          :: [[Char]] -> String
count []       =  []
count (x : xs) =  [chr (length x + 48)] ++ [head x] ++ count xs

-- group groepeert gelijke tekens in een lijst.
-- Voorbeeld: group ['1','1','4','2','2','2']  =  ["11","4","222"]
group          :: [Char] -> [[Char]]
group []       =  []
group (x : xs) =  (x : ys) : group zs
                  where (ys,zs) = span (== x) xs

--Opdracht 10 --

-- keithGetallen levert een lijst op van getallen vanaf 10 die aan de
--  voorwaarde isKeith voldoen
-- Voorbeeld: take 5 keithGetallen  =  [14,19,28,47,61]
keithGetallen :: [Int]
keithGetallen =  filter isKeith [10..]

-- isKeith levert True op als het getal een keithGetal is, en False als dat
--  niet het geval is
-- Voorbeelden: isKeith 32  =  False
--              isKeith 19  =  True 
isKeith   :: Int -> Bool
isKeith n =  volgende (toDec n) n

-- volgende kijkt of een getal daadwerkelijk een keithGetal is door de
--  volgende uit de reeks uit te rekenen en kijken of dit getal groter,
--  kleiner of gelijk aan het opgegeven getal is. Bij 'gelijk' is het een
--  keithGetal en wordt True opgeleverd
-- Voorbeeld: volgende [6,1] 61  =  True
volgende       :: [Int] -> Int -> Bool
volgende a b
   | s < b     =  volgende (tail a ++ [s]) b
   | s > b     =  False
   | otherwise =  True
       where s =  sum a
