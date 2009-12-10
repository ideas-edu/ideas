-- Functioneel Programmeren Practicum 1
-- Jan Joost Visser 3020010
-- Patrick Jansen 3117332
-- gewerkt met GHCi 6.8.2 en 6.10.1

------------------------ Hulpfuncties  ---------------------

toInt :: Int -> [Int]   ->                     Int
toInt    _      []                           = 0
toInt    b      (x:xs)  | x < 0 || x > b - 1 = -1
                        | otherwise          = x * (b ^ l) + toInt b xs
                          where l = length xs

fromInt :: Int -> Int ->            [Int]
fromInt    _      0   =             [0]
fromInt    b      x   | d == 0    = [x `rem` b]
                      | otherwise = (fromInt b d) ++ [x `rem` b]
                        where d = (x `div` b)

------------------------ Opdracht 1 ------------------------

fromDec :: [Int] -> Int
fromDec    x     =  toInt 10 x

------------------------ Opdracht 2 ------------------------

toDec :: Int -> [Int]
toDec    x   =  fromInt 10 x

------------------------ Opdracht 3 ------------------------

fromBin :: [Int] -> Int
fromBin    x     =  toInt 2 x

------------------------ Opdracht 4 ------------------------

toBin :: Int -> [Int]
toBin    x   =  fromInt 2 x

------------------------ Opdracht 5 ------------------------

fromBase :: Int -> [Char] -> Int
fromBase    b      x      | and (concat [gt,lt]) = toInt b x'
                          | otherwise = -1
                            where x' = enumerate x
                                  gt = (map (>0) x')
                                  lt = (map (<b) x')

enumerate :: [Char] -> [Int]
enumerate    []     =  []
enumerate    (x:xs) =  ((normalise x) : enumerate xs)

normalise :: Char ->                      Int
normalise    x    | x' > 47 && x' < 58  = x' - 48
                  | x' > 96 && x' < 123 = x' - 87
                  | otherwise           = -1
                    where x' = fromEnum x

------------------------ Opdracht 6 ------------------------

toBase :: Int -> Int ->                  [Char]
toBase    b      x   | b < 2 || b > 36 = "-1"
                     | otherwise       = denumerate (fromInt b x)

denumerate :: [Int] -> [Char]
denumerate []    =  []
denumerate (x:xs) = ((denormalise x) : denumerate xs)

denormalise :: Int ->                   Char
denormalise    x   | x > -1 && x < 10 = f x 48
                   | x > 9 && x < 36  = f x 87
                     where f x' b = toEnum (x + b)::Char

------------------------ Opdracht 7 ------------------------

numbers :: Int -> [String] ->            [(String, Int)]
numbers    _      []       =             []
numbers    b      (x:xs)   | x' == -1  = [] ++ (numbers b xs)
                           | otherwise = (x, x') : (numbers b xs)
                             where x'  = fromBase b x

------------------------ Opdracht 8 ------------------------

{-
Genereer met basisgetal x twee functies, 1 die van een grayCode een integer maakt, en 1 andersom
-}
grayCode :: Int -> ([Char] -> Int, Int -> [Char])
grayCode    x   =  (grayCode1 x, grayCode2 x)

{-
Zet met basisgetal n de code [Char] om in de bijbehorende Int.
Roep elem' aan met startwaarde 0, basisgetal n, code c, en startlijst strings [0..(n-1)]
-}
grayCode1 :: Int -> [Char]   -> Int
grayCode1    n      c@(x:xs) =  elem' 0 n c s
                                where s = start n

{-
elem' wordt aangeroepen met startwaarde i = 0, i is een teller. Indien gezochte code c zich reeds in
de lijst codes d bevindt, geef de index terug (middels recursieve elem' i b c xs, i++, tot c = x).
Indien de gezochte code geen element is van de lijst d, voer 1x grayList uit op d, en roep elem' daar nogmaals mee op.
-}
elem' :: Int -> Int -> [Char] -> [[Char]] -> Int
elem'    i      b      c         d@(x:xs) | (elem c d) == False = elem' (i) b c (grayList e d)
                                          | c == x = i
                                          | otherwise = elem' (i + 1) b c xs
                                            where e = map denormalise [0..(b-1)]

{-
grayCode2 zoekt bij het basisgetal n de i-ste Gray Code waarde op.
Dit wordt gedaan door de hulpfunctie grayCode2' aan te roepen met
de lijst van characters van 0 t/m n - 1, en de lijst van strings 0 t/m n - 1
-}
grayCode2 :: Int -> Int -> [Char]
grayCode2    n      i   =  grayCode2' d (start n) i
                           where d = map denormalise [0..(n - 1)]

{-
Indien de gezochte index i al binnen de lijst van strings valt, geef het i-ste element.
Anders, roep grayCode2' nogmaals aan, echter nu met de functie grayList losgelaten op de
eerdergenoemde lijst (1 iteratie van de grayCode)
-}
grayCode2' :: [Char] -> [[Char]] -> Int ->                   [Char]
grayCode2'    d         b           i   | length b - 1 >= i = b !! i
                                        | otherwise         = grayCode2' d (grayList d b) i

{-
Men neme een lijst ["0","1","2"], en reverse die: ["2","1","0"].
Neem dan het 1e element uit de lijst ['0','1','2'] en map die op kop van de 1e bovengenoemde lijst
Naam dan het 2e element uit de lijst ['0','1','2'] en map die op kop van de reversed lijst
Plak deze twee lijsten aan elkaar; ["00","01","02","12","11","10"]
Doe dit voor alle elementen uit ['0','1','2'], steeds met een reversed lijst
-}
grayList :: [Char] ->     [[Char]] -> [[Char]]
grayList    []            _        =  []
grayList    [d]           c@(x:xs) =  (map (d:) c)
grayList    d@(y:ys:yss)  c@(x:xs) =  (map (y:) c) ++ (map (ys:) (reverse c)) ++ grayList yss c

start :: Int -> [[Char]]
start    n   =  map (toBase n) [0 .. (n - 1)]

------------------------ Opdracht 9 ------------------------

{-
De functie waar om gevraagd wordt.
Maakt een [[Int]] met de Look And Say reeks, en maakt hier een lijst van [String] van.
-}
lookAndSay :: Int -> [String]
lookAndSay x = listToStringList (generateLookSay x)

{-
Pak het element waarmee begonnen moet worden, iterate de functie, waardoor je een [[Int]] krijgt.
-}
generateLookSay :: Int -> [[Int]]
generateLookSay x = iterate functie [x]

{-
Functionaliteit van de Look And Say reeks *ontbreekt*
-}
functie :: [Int] -> [Int]
functie [x] = [x]

{-
Maakt een [String] van de [[Int]] *ontbreekt*
-}
listToStringList :: [[Int]] -> [String]
listToStringList x = undefined

{-
Dups, zoals behandeld in het werkcollege.
Zet alle opeenvolgende items van een lijst in een sublijst.
[1,2,2,3,4,2] wordt [[1],[2,2],[3],[4],[2].
Is toegevoegd omdat deze bij enkele test-versies nuttig bleek.
-}
dups :: Eq a => [a] -> [[a]]
dups [] = []
dups [x] = [[x]]
dups (x:xs) = let ((y:ys):yss) = dups xs
              in if x == y then (x:y:ys):yss
                           else [x] : (y:ys):yss

------------------------ Opdracht 10 -----------------------

{-
Ga keithGetallen berekenen vanaf 10 omhoog
-}
keithGetallen :: [Int]
keithGetallen =  keith [10..]

{-
Neem het eerste getal, pas daar het keithFilter op toe.
Als het een keithGetal is dan zetten we x in de lijst, anders niet en recursief aanroepen met volgende getal.
-}
keith :: [Int]  ->                 [Int]
keith    (x:xs) |  keithFilter x = x : keith xs
                |  otherwise     = keith xs

{-
Maak een lijst van het huidige getal, geef het origineel mee en geef de lengte mee aan isKeith.
-}
keithFilter :: Int -> Bool
keithFilter    a   =  isKeith a b (length b)
                      where b = toDec a

{-
Ga deze lijst vergroten. Is het laatste element het origineel? Keith!
                         Is het laatste element groter dan het origineel? Zal geen Keith worden!
                         Is het laatste element kleiner dan het origineel? Bereken de volgende.
-}
isKeith :: Int -> [Int] -> Int ->                 Bool
isKeith    or     l        n   | (last l) == or = True
                               | (last l) > or  = False
                               | (last l) < or  = isKeith or (addNext l n) n

{-
Bereken het volgende element en voeg deze toe.
Neem n (length) laatste items (dmv take en reverse), tel deze op, en voeg deze vooraan toe.
-}
addNext :: [Int] -> Int -> [Int]
addNext    l        n   =  reverse (a : (reverse l))
                           where a = sum (take n (reverse l))