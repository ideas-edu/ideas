{---------------------------------------------------------------------------------------------

                                            Practicum 1

                                  gemaakt door Gydo van Zundert
                                               3040593

                                    code werkt in Hugs en GHC.


---------------------------------------------------------------------------------------------}

-- standaardfuncties voor omzetting van Char naar Int en omgekeerd

ord                      :: Char -> Int
ord                      =  fromEnum

chr                      :: Int  -> Char
chr                      =  toEnum

{-..........................................................................................-}

-- opdracht3: een getal dat verder vooraan staat krijgt een hogere macht van 2 met zich mee.

fromBin :: [Int] -> Int
fromBin  []    = 0
fromBin (x:xs) = 2 ^ (length (x:xs) -1 ) * x + fromBin xs

{-..........................................................................................-}

-- opdracht4: de crux van de functie is het gebruik van modulo, dit bepaald of er een 1 of 0 kom te staan.

toBin :: Int -> [Int]
toBin 0 = []
toBin x = toBin (x `div` 2) ++ [x `mod` 2]

{-..........................................................................................-}

-- opdracht1: Zelfde als opdracht3 alleen dan met tientallen.

fromDec :: [Int] -> Int
fromDec []     = 0
fromDec (x:xs) = 10 ^ (length (x:xs) -1) * x + fromDec xs

{-..........................................................................................-}

-- opdracht2: zelfde als opdracht4, maar dan met tientallen.

toDec :: Int -> [Int]
toDec 0 = []
toDec n  = toDec (n `div` 10) ++ [n `mod` 10]

{-..........................................................................................-}

-- opdracht5: generalisatie van opdracht 1 en 3. hulpfunctie makeLijst is om de String om te
--            zetten naar een lijst van getallen. Twee versies zijn er: een voor de digits en
--            de ander voor de letters. De waarde van een lijst met daarin een cijfer dat hoger
--            is dan dat de base gaat (bvb. fromBase 2 "13") is gezet naar 0.

fromBase :: Int -> [Char] -> Int
fromBase 0 xs = 0
fromBase n [] = 0
fromBase n (x:xs) | or (map (>= n) (y:ys)) = 0
                  | otherwise              = n ^ (length (x:xs) -1) * y + fromBase n xs
                       where (y:ys) = makeLijst (x:xs)

makeLijst :: [Char] -> [Int]
makeLijst []     = []
makeLijst (x:xs) = b: makeLijst xs
            where b | ord x > 96 =  ord x - (ord 'a' - 10)
                    | otherwise  =  ord x - ord '0'

{-..........................................................................................-}

-- opdracht6: generalisatie van opdracht 2 en 4. 'b' is de functie die een getal omzet naar
--            het juiste Char.

toBase :: Int -> Int -> [Char]
toBase 0 x  = []
toBase n 0 = []
toBase n x  = toBase n (x `div` n) ++ [b]
            where b | x `mod` n < 10 = chr ((x `mod` n) + 48)
                    | otherwise      = chr ((x `mod` n) + 87)

{-..........................................................................................-}

-- opdracht7: gebruikt de functie fromBase met daarbij enkele voorwaarden en werkt op lijsten.
--            aangezien fromBase 0 meegeeft als een element van een string hoger is dan de base
--            wordt de hele string niet meegenomen. De voorwaarde 'ongelijk aan "0" ', is om een
--            string die bestaat uit enkel "0" niet wordt weggelaten, aangezien 0 gedefinieerd wordt
--            in iedere base.

numbers :: Int -> [String] -> [(String,Int)]
numbers 0 (x:xs) = []
numbers n []     = []
numbers n (x:xs) | fromBase n x == 0 && x /= "0" = numbers n xs
                 | otherwise                     = (x,fromBase n x) : numbers n xs

{-..........................................................................................-}

-- opdracht9: gebruikt twee hulp functies. de eerste om recursiviteit aan te roepen en de tweede
--            rekent ook daadwerkelijk de string uit.

lookAndSay :: Int -> [String]
lookAndSay n = toBase 10 n : functie2 (toBase 10 n)

functie2 :: String -> [String]
functie2 x = b : functie2 b
              where b = functie x

functie :: String -> String
functie []     = []
functie (x:xs) = chr (lengte + ord '0'): x : functie (drop lengte (x:xs))
                 where lengte = length (takeWhile (==x) (x:xs))


{-..........................................................................................-}

-- opdracht10: deze opdracht is twee keer gemaakt. de onderstaande is niet efficient maar draagt
--             wel een functie bij zich die een keithreeks maakt. de functie maakt voor ieder getal
--             vanaf 10 een keithreeks. als het laatste element van die lijst lager is dan het input 
--             getal, wordt de lijst opnieuw gemaakt maar dan 1 element langer. dit gaat door tot 
--             het laatste element de input overstijgt. als dit getal gelijk is aan input wordt dit 
--             in de lijst gezet, anders krijgt het getal een 0 mee. uiteindelijk wordt dit gefiltert.

keithGetallen :: [Int]
keithGetallen = filter (/= 0) (lijst x)
              where x = 10

lijst :: Int -> [Int]
lijst x = lijst2 x: lijst (x+1)

lijst2 :: Int -> Int
lijst2 x | head (begin x 1) == x = x
         | otherwise            = 0

begin x n | head b < x  = begin x (n+1)
          | otherwise = b
      where b = shit x n


            shit x 1 = reverse (toDec x)
            shit x n =  element (shit x (n-1)) : shit x (n-1)


             where element c = sum (take d (c))
                   d        = length (toDec x)

{-..........................................................................................-}

-- opdracht10.2: efficiente versie. neemt telkens het aantal getallen mee dat nodig is om het
--               volgende element te berekenen. als het element kleiner is, wordt het volgende
--               element uitgerekend. als het groter is, wordt er begonnen aan de keithreeks
--               met een hogere beginwaarde. als de input gelijk is aan het element wordt het
--               in de lijst gezet.

keithGetallen2 :: [Int]
keithGetallen2 = keith2 10

keith2 :: Int -> [Int]
keith2 x = sumReeks x (toDec x)

sumReeks :: Int -> [Int] -> [Int]
sumReeks x (y:ys) | (keithGetal <  x) = sumReeks x ((ys) ++ [keithGetal])
                  | (keithGetal == x) = x: keith2  (x+1)
                  | (keithGetal >  x) = keith2 (x+1)
                  where keithGetal = (y + sum(ys))


{-....................................THE END...............................................-}















