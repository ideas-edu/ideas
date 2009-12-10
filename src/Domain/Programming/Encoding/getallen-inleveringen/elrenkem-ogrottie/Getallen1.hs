
{-  Dit bestand is het werk van Otto Rottier en Esger Renkema. Gebruikt
    als compiler is GHC versie 6.10.
 -}

module Getallen1 where

    import Data.Char
    import Data.List

{-  We kunnen de functie fromIets makkelijk veralgemeniseren. Dat doen 
    we dan ook, en drukken fromBin en fromDec daarin uit. We controleren 
    niet of de lijst wel werkelijk hetzelfde grondtal heeft: fromBin [4]
    levert gewoon 4 op. 
 -}

    fromIntBase :: Int -> [Int] -> Int
    fromIntBase b = foldl' (\ x y -> b*x+y ) 0   

    fromDec :: [Int] -> Int
    fromDec = fromIntBase 10

    fromBin :: [Int] -> Int
    fromBin = fromIntBase 2

{-  De gevraagde functie fromBase verschilt alleen van de eerder 
    gevonden veralgemenisatie in dat het met lijsten van tekens werkt.
    We kunnen fromBase uitdrukken in fromIntBase als we eerst letters
    naar cijfers kunnen omzetten -- daarvoor definieren we charToInt.
 -}

    charToInt :: Char -> Int
    charToInt x
        | isDigit x = ord x - ord '0'
        | isLower x = ord x - ord 'a' + 10
        | otherwise = error "vreemd getal"


    fromBase :: Int -> [Char] -> Int
    fromBase b n = fromIntBase b $ map charToInt n

{-  Nu beginnen we de ander kant op. Onze aanpak is geheel analoog aan 
    die van de from-functies.
 -}

    toIntBase :: Int -> Int -> [Int]
    toIntBase b x = reverse . map (`rem` b) . takeWhile (/=0) 
        $ iterate (`div` b) x 

    toDec :: Int -> [Int]
    toDec = toIntBase 10

    toBin :: Int -> [Int]
    toBin = toIntBase 2

    intToChar :: Int -> Char
    intToChar x
        | x<10 = chr (x + ord '0')
        | x>9  = chr (x + ord 'a' - 10)

    toBase :: Int -> Int -> [Char]
    toBase b n = map intToChar $ toIntBase b n 

{-  Sommige van de bovenstaande functies gebruiken we in de definitie van de
    functie numbers. We moeten hiervoor controleren of het nummer wel het
    juiste grondtal heeft.
 -}

    numbers :: Int -> [[Char]] -> [(String,Int)]
    numbers b l = [(x,fromBase b x) | x<-l, and $ map ((<b).charToInt) x ]

{-  We hebben nu de eerste zeven vragen gehad. Volgen er drie. Voor het
 -  maken van de Gray-functie stellen we eerst de twee losse functie op.
 - 
 -  Het volgende algoritme is gebruikt:
 -
 -  Noem  o  het oorspronkelijke en  a  het gecodeerde getal. Hun 
 -  decimalen zijn  o[1]...o[n]  en  a[1]...a[n] . Het grondtal noemen
 -  we  b  en we schrijven  z = x mod y  voor het kleinste niet-negatieve
 -  getal  z  zodat  y | (z-x) .
 -
 -  Voor het coderen en het decoderen gelden dan de simpele formules
 -  a[i] = o[i] - o[i-1] mod b  en  o[i] = a[i] + o[i-1] mod b . Het enige
 -  dat nu nog nodig is, is dat  o[1] = a[1] .
 -}

    naarGray :: Int -> Int -> [Char]
    naarGray _ 0 = "0" -- anders wordt het ""
    naarGray b n = hulp (toIntBase b n) 0
        where
            hulp [] _ = ""
            hulp (i:is) j = intToChar (rem (i-j+b) b) : hulp is i


    vanGray :: Int -> [Char] -> Int
    vanGray b c = fromIntBase b $ hulp (map charToInt c) 0 
        where
            hulp [] _ = []
            hulp (i:is) j = k : hulp is k
                where
                    k = rem (i+j) b

    grayCode :: Int -> ([Char] -> Int, Int -> [Char])
    grayCode b = (vanGray b, naarGray b)

{-  Nu de reeksen van Conway. Die zijn makkelijk te programmeren als
 -  we niet naar de cijfers kijken, maar slechts naar een reeks symbolen.
 -  Om van Int naar [[Char]] te gaan definieren we dus een extra functie.
 -}

    nextLookAndSay :: [Char] -> [Char]
    nextLookAndSay n = foldl' (\ x y -> x++[intToChar(length y), head y]) 
        [] (group n)

    lookAndSay :: Int -> [[Char]]
    lookAndSay n = iterate nextLookAndSay (toBase 10 n) 

{-  Tot slot de Keith-getallen. Ook hier definieren we een hulpfunctie.
 -}

    keithReeks :: Int -> [Int]
    keithReeks n = l ++ (hulp l)
        where
	    l = toDec n
	    hulp x@(_:xs) = s : (hulp (xs ++ [s]))
	        where
		    s = sum x

    keithGetallen :: [Int]
    keithGetallen = [n | n<-[14..], 
        n == last ( takeWhile (<=n) (keithReeks n)) ]




