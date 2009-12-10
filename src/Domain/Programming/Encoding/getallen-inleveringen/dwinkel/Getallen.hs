{-
	Functioneel Programmeren - Practicum 1
	Getallen

	Dennis Winkel - dwinkel - 3220656
	
	Werkend met Helium en GHCi 6.10.1
-}

-- Opgave 1 --
-- Maakt een decimaal getal uit een rij van decimale getallen
fromDec :: [Int] -> Int
fromDec [] = 0
fromDec (x:xs) = 10^length(xs) * x + fromDec xs

-- Opgave 2 --
-- Maakt een rij getallen uit een decimaal getal
toDec :: Int -> [Int]
toDec 0 = []
toDec x =  toDec ((x - lastDigit) `div` 10) ++ [lastDigit]
           where lastDigit = x `mod` 10

-- Opgave 3 --
-- Maakt een decimaal getal van een binaire representatie
fromBin :: [Int] -> Int
fromBin [] = 0
fromBin (x:xs) = x * (2^(length xs)) + fromBin xs

-- Opgave 4 --
-- Maakt van een decimaal getal een binaire representatie
toBin :: Int -> [Int]
toBin 0 = []
toBin x = toBin ((x - lastDigit) `div` 2) ++ [lastDigit]
           where lastDigit = x `mod` 2

-- Opgave 5 --
-- Maakt van een grondgetal en een string de bijbehorende waarde
fromBase :: Int -> [Char] -> Int
fromBase _ [] = 0
fromBase g (x:xs) = (nr * g^length(xs)) + (fromBase g xs)
                    where nr = if (elem x ['0'..'9']) 
     				           then (fromEnum x) - 48 
							   else (fromEnum x) - 87
							  
-- Opgave 6 --
-- Produceert voor een grondgetal 2 t/m 36 en een getal de bijbehorende cijferreeks
toBase :: Int -> Int -> [Char]
toBase g x = toChar (toInt g x)

toInt :: Int -> Int -> [Int] -- Zet de getallen om in een lijst van getallen
toInt _ 0 = []
toInt 0 _ = []
toInt g x =  toInt g ((x - lastDigit) `div` g) ++ [lastDigit]
              where lastDigit = x `mod` g

toChar :: [Int] -> String -- Zet de lijst van getallen om in een string
toChar [] = ""
toChar (x:xs) = if elem x [0..9]
                then (toEnum (x + 48)) : (toChar xs)
				else (toEnum (x + 87)) : (toChar xs)

-- Opgave 7 --
-- Maakt voor een grondgetal 2 t/m 36 en een lijst met woorden een tuple met de woorden die overeenkomen
numbers :: Int -> [String] -> [(String,Int)]
numbers _ [] = []
numbers g (x:xs) = makeTuple g (filter (isValid g) (x:xs))

isValid :: Int -> String -> Bool -- Kijkt of de letters binnen het domein van het grondgetal vallen
isValid _ [] = True
isValid g (x:xs) | ((fromEnum x) - 87) > g = False
                 | otherwise = isValid g xs

makeTuple :: Int -> [String] -> [(String,Int)] -- Maakt de gewenste tuple uit het grondgetal en de valide lijst
makeTuple _ [] = []
makeTuple g (x:xs) = (x,fromBase g x) : makeTuple g xs

-- Opgave 8 -- Helaas niet gelukt
-- Gray-codering
{-
           .--'''''''''--.
        .'      .---.      '.
       /    .-----------.    \
      /        .-----.        \
      |       .-.   .-.       |
      |      /   \ /   \      |
       \    | .-. | .-. |    /
        '-._| | | | | | |_.-'
            | '-' | '-' |
             \___/ \___/
          _.-'  /   \  `-._
        .' _.--|     |--._ '.
        ' _...-|     |-..._ '
               |     |
               '.___.'
                 | |
-}
				 
-- Opgave 9 --
-- Produceert voor een gegeven getal een look-and-say reeks
lookAndSay :: Int -> [String]
lookAndSay x = map show (map fromDec (numCounterList [x]))

numCounterList :: [Int] -> [[Int]] -- Produceert een oneindige lijst met resultaten van de functie numCounter
numCounterList (x:xs) = a : numCounterList a
                        where a = numCounter (x:xs)

numCounter :: [Int] -> [Int] -- Telt hoevaak een bepaalde waarde voorkomt in een lijst
numCounter [] = []
numCounter (x:xs) = (length (takeWhile (==x) (x:xs))) : x : (numCounter (dropWhile (==x) (xs)))

-- Opgave 10 --
-- Genereerd een oneindige lijst van Keithgetallen
keithGetallen :: [Int]
keithGetallen = [x | x <- [10..], isKeithNumber x (keithNumberList [10..])]

isKeithNumber :: Int -> [Int] -> Bool -- Controleert of het getal een KeithNumber is
isKeithNumber _ [] = False
isKeithNumber g (x:xs) = if (g < x) 
                         then False 
						 else if (g == x) 
						 then True 
						 else isKeithNumber g (xs ++ [y])
                         where y = foldr (+) 0 (x:xs)

keithNumberList :: [Int] -> [Int] -- Maakt een lijst van getallen die voldoen aan isKeithNumber
keithNumberList [] = []
keithNumberList (x:xs) = if (isKeithNumber x (toDec x)) 
                         then x : keithNumberList xs 
						 else keithNumberList xs