module Getallen where

import Char

-- Student: Ruvar Spauwen
-- StudentID: 3223574
-- Date: 02-03-2009


{- Opgave 1
FromDec maakt van een lijst van enkele getallen een decimaal getal
Foldl zorgt dat het start getal (0) met 10 wordt vermenigvuldigd en bij eerste getal uit de lijst wordt opgeteld,
Vervolgens wordt dit resultaat ook met 10 vermenigvuldigd en bij het volgende getal opgeteld, enz. 
-}
fromDec :: [Int] -> Int
fromDec = foldl (\x y -> x*10 + y) 0

{- Opgave2
ToDec maakt van een decimaal getal een lijst van enkele cijfers:
toDec wordt recursief aangeroepen met een factor tien kleiner getal en
vervolgens wordt de rest van het getal `mod` 10 toegevoegd aan de lijst.
-}
toDec :: Int -> [Int]
toDec 0      = []
toDec c      = toDec (c `div` 10) ++ [c `mod` 10]

{- Opgave 3
FromBin maakt van een lijst binaire getallen een enkel decimaal getal.
Eerst checkt "isBinair" of de gegeven lijst wel alleen uit nullen en enen bestaat.
Zo ja, dan wordt via foldl het juist getal berekend:
Vermenigvuldig het basisgeval met 2 plus het eerste getal uit de lijst, 
vermenigvuldig dit resultaat weer met 2 plus het tweede getal uit de lijst, enz.
-}
fromBin :: [Int] -> Int
fromBin cs = if isBinair cs then foldl (\ x y -> x*2 + y) 0 cs else error "Invoer is niet Binair!"

-- Hulpfunctie: controleert of een lijst uit binaire getallen bestaat.
isBinair :: [Int] -> Bool
isBinair [] = True
isBinair (c:cs) | c == 1 || c == 0 = isBinair cs
                | otherwise = False

{- Opgave 4
ToBin maakt van een decimaal getal een lijst van binaire getallen.
Dit werkt het zelfde als toDec maar dan inplaats van 10, een 2 overal...
-}
toBin :: Int -> [Int]
toBin 0 = []
toBin c | c > 0     = toBin (c `div`2) ++ [c `mod` 2]
        | otherwise = error "Invoer is niet geschikt"

{- Opgave 5
FromBase is de algemene versie van fromDec en fromBin.
Hij verwacht een getal, die het tallig stelsel bepaald en een lijst van characters.
De functie maakt onderscheid tussen stelsels groter en kleiner of gelijk aan 10.
Omdat bij het omwisselen van Char naar Int verschillende waardes worden gebruikt bij de letters dan bij de getallen.
-}
fromBase :: Int -> [Char] -> Int
fromBase 0 _ = 0
fromBase _ []= 0
fromBase n  cs | n <= 10 && n >= 2  = foldl (\x y -> x*n + (ord y - 48)) 0 cs
               | n > 10  && n <= 36 = foldl (\x y -> if isDigit y then x*n + (ord y - 48)
                                                                  else x*n + (ord y - 87) ) 0 cs
               | otherwise = error "Invoer is niet geschikt"


{- Opgave 6
ToBase is de algemene versie van toDec en toBin.
Hij verwacht ook een getal, die het tallig stelsel bepaald en nog een getal, die moet worden omgerekend.
Deze algemene versie lijkt wederom erg veel op de specifieke versies, maar omdat er nu weer gebruik kon worden gemaakt van stelsels groter dan 10,
moest er weer worden gegocheld bij het omwisselen van Int naar Char.
Dus bij een 16 talligstelsel moet steeds worden gecontroleerd of het huidige getal wel een letter of een getal character moet worden in de lijst.
-}
toBase :: Int -> Int -> [Char]
toBase 0 _ = []
toBase _ 0 = []
toBase n c | n <= 10 && n >= 2  = toBase n (c `div` n) ++ [chr (c `mod` n + 48)]
           | n >  10 && n <= 36 = let m = c `mod` n in toBase n (c `div` n) ++ [if m < 10 then chr (m+48) else chr (m+87)]
           | otherwise = error "Invoer is niet geschikt"

{- Opgave 7
Numbers verwacht een getal, voor het tallig stelsel, en een lijst met Strings.
Indien alle characters van een String binnen het tallig stelsel vallen, wordt de String aan de resultaal lijst toegevoegd,
plus het overeenkomend getal.
Numbers gebruikt de hulpfunctie checkList, die kijkt voor elke String of de functie moet worden toegevoegd aan het resultaat.
Checklist kijkst voor elke character in de String of indien het een getal is of het getal binnen het gekozen stelsel zit,
indien het geen getal is wordt d.m.v. een correctie waarde ook gekeken of de letter in het stelsel zit.
-}
numbers :: Int -> [String ] -> [(String, Int)]
numbers 0 _  = []
numbers _ [] = []
numbers n (c:cs) | checkList c = [(c,fromBase n c)] ++ numbers n cs
                 | otherwise   = numbers n cs
                      where  checkList [] = True
                             checkList (l:ls) | isDigit l && ord l < n = checkList ls
                                              | not (isDigit l) && ord l - 87 <= n = checkList ls
                                              | otherwise = False


{- Opdracht 8
Helaas is het me niet gelukt om deze opdracht op tijd af te krijgen, omdat ik heel lang bezig was met een verkeerde manier.
De decimale GreyCode, die de bijbehorende cijferreeks produceert, is uitiendelijk wel gelukt.
Het probeel waar ik heel lang op stuite was: ik probeerde fromBase te gebruiken inplaats van show, maar de standaar fromBase geeft een lege lijst bij een '0'.
Indien ik dit zou aanpassen in fromBase, zou de standaard versie niet werken.
-}

--TODO
--grayCode :: Int -> ([Char] -> Int, Int -> [Char])
--grayCode n =

--HulpFuncties
--createGreyNumber :: [Char] -> Int
--createGreyNumber [_] = 0

createGreyList :: Int -> [Char]
createGreyList n = let s = take n (iterate (+1) 0)
                   in foldr (:) "" (concat(map show s)) ++ calcGC s
                   where calcGC :: [Int] -> [Char]
                         calcGC xs = let ys = map (\x -> (x+1) `mod` n) xs
                                     in foldr (:) "" (concat(map show ys)) ++ calcGC ys



{- Opdracht 9: Look-And-Say
LookAndSay geeft een reeks getallen waarin elk element van de reeks
een `beschrijving' van het vorige element vormt.
De hulpfunctie look telt in de lijst van characters het aantal opeenvolgende dezelfde characters
en stopt de paren van waardes en aantallen in het resultaat
-}
lookAndSay :: Int -> [String]
lookAndSay 0 = []
lookAndSay l = [show l] ++ lookAndSay (fromDec (look (show l) 0))
               where look :: [Char] -> Int -> [Int]
                     look []     _ = []
                     look (x:xs) c | xs /= [] && x == head xs = look xs (c+1)
                                   | otherwise = (c+1):((ord x - 48): (look xs 0))

                                   
{- Opdracht 10: Keith-Getallen
keithGetallen produceert een oneindige lijst van Keith getallen:
Een natuurlijk getal is een Keith-getal als het groter is dan 9 en zelf voorkomt in
de reeks die het op bovenstaande manier genereert. Dus: 14 en 197 zijn Keithgetallen
(want ze komen zelf in de door hen genereerde reeks voor), maar 123 is
dat niet (want 123 komt niet in zijn eigen reeks voor).

Er worden 2 hulpfuncties gebruikt:
- calcKeith: berekent van een basisgetal, de specifieke getallen reeks, 
waarin uiteindelijk moet worden gekeken of het basisgetal een Keithgetal heeft

- getKeith: kijkt in de zojuist geproduceerde reeks,
of de concatenatie van het basisgetal voorkomt in de reeks.
-}
keithGetallen :: [Int]
keithGetallen = filter (>0) (map (getKeith . calcKeith . toDec) [10..])


calcKeith :: [Int] -> ([Int],[Int])
calcKeith x = (x, x ++ iterateKeith (length x) x)
                where  iterateKeith :: Int -> [Int] -> [Int]
                       iterateKeith n l =  let t = sum l
                                           in  t : iterateKeith n (tail l ++ [t])

getKeith :: ([Int], [Int]) -> Int
getKeith (xs, ys) | elem2 (fromDec xs) ys = fromDec xs
                  | otherwise = 0
                    where elem2 :: Ord a => a -> [a] -> Bool
                          elem2 _ [] = False
                          elem2 e (z:zs) | e <  z    = False
                                         | e == z    = True
                                         | otherwise = elem2 e zs
                                         
                                         

