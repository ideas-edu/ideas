{-
Getallen practicum opgave voor Functioneel Programmeren
Taco Steemers, 3151239
Hiervoor is GHCi versie 6.8.3 gebruikt.
-}

import Char

{-
fromDec [4, 2, 3] geeft 423.
fromDec [4, 11, 3] geeft 513.

Ik heb hier uit oogpunt van de computatie kosten gekozen om een hulpfunctie te maken inplaats van length elke keer aan te roepen.
-}
fromDec :: [Int] -> Int
fromDec v = fromDec' v ((length v) - 1)

fromDec' :: [Int] -> Int -> Int
fromDec' (h:[]) _   = h
fromDec' (h:t) l = (h * (10 ^ l)) + (fromDec' t (l - 1))

{-
toDec 423 geeft [4; 2; 3].
-}
toDec :: Int -> [Int]
toDec 0 = []
toDec v = (toDec (div v 10)) ++ [mod v 10]  

{-
fromBin [1; 0; 0; 1; 1; 0] geeft 38.

Ik heb hier uit oogpunt van de computatie kosten gekozen om een hulpfunctie te maken inplaats van length elke keer aan te roepen.
-}
fromBin :: [Int] -> Int
fromBin v = fromBin' v ((length v) - 1)

fromBin' :: [Int] -> Int -> Int
fromBin' (h:[]) _   = h
fromBin' (h:t) l = (h * (2 ^ l)) + (fromBin' t (l - 1))

{-
toBin 38 geeft [1, 0, 0, 1, 1, 0].
-}
toBin :: Int -> [Int]
toBin 0 = []
toBin v = (toBin (div v 2)) ++ ((mod v 2) : [])

{-
fromBase 10 "423" geeft 423;
fromBase 2 "100110" geeft 38;
fromBase 16 "f4c8" geeft 62664;
fromBase 23 "5j" geeft 134.
-}
fromBase :: Int -> [Char ] -> Int
fromBase b a = fromBase' b a ((length a) - 1)

fromBase' :: Int -> [Char] -> Int -> Int
fromBase' _ (h:[]) _   = (charToInt h)
fromBase' b (h:t) l = ((charToInt h) * (b ^ l)) + (fromBase' b t (l - 1))


charToInt :: Char -> Int
charToInt v | v >= '0' && v <= '9' =  ord v - ord '0'      -- trek ord '0' af zodat (charToInt '0') ook echt 0 teruggeeft
            | v >= 'a' && v <= 'z' =  ord v - ord 'a' + 10 -- trek ord 'a' af en tel er 10 bij op zodat (charToInt 'a') ook echt 10 teruggeeft
            | otherwise            =  error "charToInt: input valt buiten 0-9 en a-z"

{-
toBase 10 423 geeft "423";
toBase 2 38 geeft "100110";
toBase 16 62664 geeft "f4c8";
toBase 23 134 geeft "5j".
-}

intToChar :: Int -> Char
intToChar v | v >= 0  && v <=  9   =  chr (ord '0' + v)
            | v >= 10 && v <= 35   =  chr (ord 'a' + v - 10)
            | otherwise            =  error "charToInt: input valt buiten 0-35"

toBase :: Int -> Int -> String
toBase _ 0 = []
toBase b v = toBase b (div v b) ++ [(intToChar (mod v b))]


{-
numbers 16 ["ace"; "face"; "faces"] geeft [("ace"; 2766); ("face"; 64206)].
-}

numbers :: Int -> [String] -> [(String, Int)]
numbers _ [] = []
numbers b (h:t)  | (numbers' b h) = (h, (fromBase b h)) : (numbers b t)
                 | otherwise      = (numbers b t)

numbers' :: Int -> String -> Bool
numbers' _ [] = True
numbers' b (h:t) | i>1 && i<b = (numbers' b t)
                 | otherwise  = False
                 where i = (charToInt h)


{-
GrayCode
Samengewerkt met BT van Dijk, 3173755
-}


grayCode :: Int -> ([Char] -> Int, Int -> [Char])
grayCode b = (fromGrayCode b, toGrayCode b)


-- toGrayCode
-- om tegelijkertijd naar base 'b' en van base 'b' naar gray code met 'b' cijfers te kunnen gaan wordt van links naar rechts gewerkt
-- eerst wordt bekeken wat de grootste macht van de base is die in het getal past
--     bv. de macht 3 bij de base 2 voor het getal 14 (2^3 past in 14, 2^4 niet)
--     dit getal wordt gebruikt als startbasis. als de base 3 is dan wordt het getal 4 cijfers lang
--     zonder dit te berekenen kun je niet van links naar rechts over de getallen heen gaan
-- om te kijken welk getal op welke positie moet wordt gekeken hoevaak de huidige macht in het huidige getal past
--     bv. de macht 3 bij de base 3 geeft het getal 27, dat twee maal in het huidige getal 75 past
--     dan wordt op de huidige positie het cijfer 2 genoteerd, en de functie zal de eerstvolgende keer aangeroepen worden met de macht 2 en het getal 21 (75-27-27)
-- nadat bekend is welk getal op de huidige positie moet vind er een verschuiving plaats om van standaard base x naar een graycode met x getallen te gaan
--     voor deze verschuiving is een getal nodig, hier genoteerd als 's', voor shift
--     deze shift begint bij 0 voor de MSB, en neemt bij elke volgende aanroep van de functie toe met het huidige, al geshifte getal
toGrayCode :: Int -> Int -> [Char]
toGrayCode b v = toGrayCode' b v (greatestPowerOf b v) 0

toGrayCode' _ _ (-1) s = []
toGrayCode' b v p s | v<(b^p)   = ((intToChar (mod (b-s) b)) : toGrayCode' b v (p-1) (s+(mod (b-s) b)))
                    | otherwise = let a = (amountOfFits v (b^p))
                                  in  ((intToChar (mod (a+b-s) b)) : toGrayCode' b (v - ((b^p)*a)) (p-1) (s+(mod (a+b-s) b)))

-- hoe vaak past y in x?
amountOfFits :: Int -> Int -> Int
amountOfFits x y | x==0 || y==0 = error "amountOfFits: aangeroepen met een of twee nullen als argumenten"
                 | otherwise    = amountOfFits' x y 0
amountOfFits' :: Int -> Int -> Int -> Int
amountOfFits' x y z | (x-y)<0   = z
                    | otherwise = amountOfFits' (x-y) y z+1

-- wat is de grootste macht van de base die in het getal past? Voorbeeld: (greatestPowerOf 2 14) levert 3, want 2^3=8 en 2^4=16
greatestPowerOf :: Int -> Int -> Int
greatestPowerOf b v = greatestPowerOf' b v 1
greatestPowerOf' :: Int -> Int -> Int -> Int
greatestPowerOf' b v p | v<(b^p)  = (p-1)
                       | otherwise = greatestPowerOf' b v (p+1)



-- deze implementatie van fromGrayCode zoekt in toGrayCode naar het gezochte getal.
-- met wat rekenwerk is de zoekruimte verkleind, maar de implementatie is nog steeds niet ideaal
fromGrayCode :: Int -> [Char] -> Int
fromGrayCode b (c:xs) = fromGrayCode' (map (toGrayCode b) (0:[(b^(length xs))..])) ((b^(length xs))-1) (c:xs)

fromGrayCode' :: [[Char]] -> Int -> [Char] -> Int
fromGrayCode' (x:xs) count doel | x==doel = count
                                | otherwise = fromGrayCode' xs (count+1) doel


{-
LookAndSay
Samengewerkt met BT van Dijk, 3173755

Schrijf een functie
lookAndSay :: Int -> [String ]
die gegeven een eerste element de bijbehorende (oneindige) look-and-say-reeks
produceert. Bijvoorbeeld: take 6 (lookAndSay 1) geeft
["1"; "11"; "21"; "1211"; "111221"; "312211"]:
-}


lookAndSay :: Int -> [String]
lookAndSay num = ([intString num] ++ lookAndSay (fromDec (lookAndSayHulp (0:[0]) (toDec num)))) 

lookAndSayHulp :: [Int] -> [Int] -> [Int]
lookAndSayHulp lijst [] = lijst
lookAndSayHulp [] _ = error "lookAndSayHulp: aangeroepen met een lege linker lijst"
lookAndSayHulp (count:vorige) (x:xs)    | [x]==vorige = lookAndSayHulp ((count+1):vorige) xs
                                        | otherwise = (count:vorige) ++ lookAndSayHulp (1:[x]) xs

-- Over genomen uit reader, blz 66, om een int naar een string om te zetten
intString :: Int -> [Char]
intString = map digitChar
                .reverse
                .map (`rem`10)
                .takeWhile(/=0)
                .iterate(`div`10)
digitChar :: Int -> Char
digitChar n = chr (n + ord '0')

{-
Keithgetallen 
Samengewerkt met BT van Dijk, 3173755

Bijvoorbeeld:
take 5 keithGetallen
geeft [14; 19; 28; 47; 61]
-}

keithGetallen :: [Integer]
keithGetallen = concatMap keithGetallen' [10..]

keithGetallen' :: Int -> [Integer]
keithGetallen' x = keithGetallenHulp (map toInteger (toDec x)) (toInteger x)

keithGetallenHulp :: [Integer] -> Integer -> [Integer]
keithGetallenHulp [] _ = error "keithGetallenHulp: aangeroepen met een lege linker lijst"
keithGetallenHulp (x:xs) doel   | (sum (x:xs))==doel = [doel]
                                | (sum (x:xs)) > doel = []
                                | otherwise = keithGetallenHulp (xs ++ [sum (x:xs)]) doel