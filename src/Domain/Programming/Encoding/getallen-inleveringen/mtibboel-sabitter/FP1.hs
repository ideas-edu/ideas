{- Team Foxtrot Papa :
Mark Tibboel  0428701
Steven Bitter 0448052 
Compiler: Helium
-}

-- opgave 1 Negatieve getallen kunnen worden weergegeven door het eerste getal negatief te maken. 
-- Andere negatieve getallen worden genegeerd. Een lege lijst beschouwen wij als 0. We draaien de lijst
-- eerst om zodat we slechts 2 maal door de lijst hoeven tov gebruik van length oid.
fromDec :: [Int] -> Int
fromDec [] = 0
fromDec (x:xs) | x < 0 = -1 * fromDex(reverse (x:xs))
               | otherwise = fromDex(reverse (x:xs))

fromDex :: [Int] -> Int
fromDex [] = 0
fromDex (x:xs) = abs x + 10 * fromDex xs

-- opgave 2 Ook hier worden negatieve getallen verwerkt door in de lijst het eerste getal negatief weer te geven.
toDec :: Int -> [Int]
toDec x | x < 10 && x > (-10) = [x]
        | x > 10 = toDec (x `div` 10) ++ [x `mod` 10]
        | otherwise = toDec ((-1) * ((abs x) `div` 10)) ++ [(abs x) `mod` 10]

-- opgave 3  Deze opgave kan met opgave 1 gebruik maken van dezelfde functie maar dat doen we niet gezien opgave 5
fromBin :: [Int] -> Int
fromBin [] = 0
fromBin xs = fromBim (reverse xs)

fromBim :: [Int] -> Int
fromBim [] = 0
fromBim (x:xs) | x < 0 || x > 1 = error "Niet binair getal!"
               | otherwise = x + 2 * fromBim xs

-- opgave 4 Deze opgave kan met opgave 2 gebruik maken van dezelfde functie maar dat doen we niet gezien opgave 5
toBin :: Int -> [Int]
toBin x | x < 0 = error "negative int"
        | x < 2 = [x]
        | otherwise = toBin (x `div` 2) ++ [x `mod` 2]

--opgave 5 We checken niet op de base 2-36 aangezien base 1 slechts 1 getal kan weergeven (tenzij je 1 gebruikt ipv nul wat wij doen)
--en de gebruikte characters voor base 0 en 36+ worden geweigerd
fromBase :: Int -> [Char] -> Int
fromBase _ [] = 0
fromBase y xs = fromBaze y (reverse xs)

fromBaze :: Int -> [Char] -> Int
fromBaze _ [] = 0
fromBaze y (x:xs) = fromEnun y x + y * fromBaze y xs

fromEnun :: Enum a => Int -> a -> Int
fromEnun y x | z >= 48 && z <= 57 && z < y + 48 = z - 48
             | z >= 97 && z <= 122 && z < y + 87 = z - 87
             | otherwise = error "illegal character"
             where z = fromEnum x

-- opgave 6
toBase :: Int -> Int -> [Char]
toBase y x | x < 0 = error "negative int"
           | x < y = [toEnun y x]
           | otherwise = toBase y (x `div` y) ++ [toEnun y (x `mod` y)]

toEnun :: Int -> Int -> Char
toEnun y x | x >= 0 && x < 10 = toEnum ( x + 48)
           | x > 9 && x < y = toEnum (x + 87)
           | otherwise = error "illegal character"

-- opgave 7
-- Het is ons niet gelukt om er voor te zorgen dat de woorden die geen getallen voorstelden er uit te vissen.
-- In plaats daarvan krijgen we de foutmelding die de functie fromEnun produceert.
numbers :: Int -> [String ] -> [(String, Int)]
numbers _ [] = []
numbers y xs = map (numberz y) xs

numberz :: Int -> String -> (String, Int)
numberz _ [] = ("",0)
numberz y xs | fromBase y xs > 0 = (xs, fromBase y xs)
             | otherwise = ("",0)

-- opgave 8 Als het getal kleiner is dan de basis, dan kan het getal zelf opgeleverd worden.
-- Het getal wordt daarna opgebroken in 2 stukken: Het achterste nummer en de rest.
-- De getallen in onze graycodering zijn afwisselend op- en aflopend, met de check even op de rest wordt er gekeken of er op- of af-gelopen moet worden.
-- Daarna wordt de achterste waarde berekent en achter de recursieve aanroep geplakt.

grayCode :: Int -> ([Char]->Int,Int->[Char])
grayCode y | checkBase y = (fromGray y, toGray y)
           | otherwise = error "illegal base"

toGray :: Int -> Int -> [Char]
toGray y x | x < y = [toEnun y x]
           | x >= y && even a = toGray y a ++ [toEnun y b]
           | otherwise = toGray y a ++ [toEnun y (y - 1 - b)]
               where a = x `div` y
                     b = x `mod` y

fromGray :: Int -> [Char] -> Int
fromGray y xs = fromBase y (fromGr4y y xs)

fromGr4y :: Int -> [Char] -> [Char]
fromGr4y _ [] = []
fromGr4y _ (x:[]) = [x]
fromGr4y y (x:xs:xss) | even (fromEnun y x) = x : fromGr4y y (xs:xss)
                      | otherwise = x : fromGr4y y (toEnun y (y - 1 - fromEnun y xs):xss)


checkBase :: Int -> Bool
checkBase y | 1 < y && y < 37  = True
            | otherwise = False

-- opgave 9

lookAndSay :: Int -> [String]
lookAndSay x = (intString x) : initLookAndSay x

initLookAndSay :: Int -> [String]
initLookAndSay x = buildLookAndSay (z) : initLookAndSay (stringInt (buildLookAndSay (z)))
                   where z = intString x

buildLookAndSay :: String -> String
buildLookAndSay [] = []
buildLookAndSay (x:[]) = '1' : x : []
buildLookAndSay (x:xs) = (intString z) ++ x : buildLookAndSay (drop z (x:xs))
                               where z = count(x:xs)

-- Telt hoevaak de eerste char van een string achter elkaar voorkomt en levert daarvan een Int op
count :: String -> Int
count [] = 0
count [_] = 1
count (x:xs:xss) | x == xs = 1 + count (xs:xss)
                 | otherwise = 1

-- Zet een Int om naar een String met daarin het getal als chars
intString :: Int -> String
intString x | x < 10 = [primChr (x + 48)]
            | otherwise = intString (x `div` 10) ++ [primChr (x `mod` 10 + 48)]

-- Zet een String met chars van getallen om naar een Int
stringInt :: String -> Int
stringInt xs = stringInt' (reverse xs)

stringInt' :: String -> Int
stringInt' [] = 0
stringInt' (x:xs) = primOrd x - 48 + 10 * stringInt' xs

-- opgave 10 De functie wordt geinitialiseerd met begin getal 10 (0-9 zijn niet mogelijk) checkKeith kijk of
-- een getal een keith getal is. Er wordt ook gekeken of de lijst die een getal produceert ergens heen gaat (bijv 100)
-- Ook hebben wij nieuw leven in de functie elemBy geblazen :)
keithGetallen :: [Int]
keithGetallen = initKeith 11

initKeith :: Int -> [Int]
initKeith x | keithGetal x = x : initKeith (x+1)
            | otherwise    = initKeith (x+1)

keithGetal :: Int -> Bool
keithGetal x  = checkKeith x (toDec x)

checkKeith :: Int -> [Int] -> Bool
checkKeith _ [] = False
checkKeith x (y:ys) | x < y = False
                    | elemBy (==) x (y:ys) = True
                    | sum ys == 0 = False
                    | otherwise = checkKeith x (ys ++ [sum (y:ys)])

                    
elemBy :: (a->a-> Bool) -> a -> [a] -> Bool
elemBy _ _ [] = False
elemBy test x (y:ys) | test x y = True
                     | otherwise = elemBy test x ys






