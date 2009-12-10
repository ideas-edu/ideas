module Practicum1 where
import Char

-- Gemaakt door: Dafne van Kuppevelt, Studentnummer 3370836
-- Gemaakt mbv Helium

--Opgave 1
--fromDec keert de lijst eerst om, en laat er dan een foldr op los.
fromDec :: [Int] -> Int
fromDec l = foldr op 0 (reverse l)
              where op a b = a + 10*b


--Opgave 2
--toDecReverse neemt telkens de laatste decimaal, en plakt die vooraan de lijst, en gaat verder met het oude getal gedeeld door 10. 
--Dit levert een omgekeerde lijst op, dus de lijst wordt nog een keer omgekeerd.
toDec :: Int -> [Int]
toDec n = reverse (toDecReverse n)
                where toDecReverse m = if m < 10 then [m] else (m-x*10) : toDecReverse x
                                                               where x = div m 10
 
--Opgave 3
--fromBin gaat op dezelfde manier te werk als fromDec, maar dan met 2 als grondgetal in plaats van 10
fromBin :: [Int] -> Int
fromBin l = foldr op 0 (reverse l)
              where op a b = a + 2*b

--Opgave 4
--toBin gaat het zelfde te werk als toDec, maar dan met 2 ipv 10
toBin :: Int -> [Int]
toBin n = reverse (toBinReverse n)
                where toBinReverse m = if m < 2 then [m] else (m-x*2) : toBinReverse x
                                                               where x = div m 2

--Opgave 5
--Ik definieer eerst een hulpfunctie IntegerList, die van een string een lijst met integers maakt
toIntegerList :: [Char] -> [Int]
toIntegerList [] = []
toIntegerList (a:as) |ord '0' <= ord a && ord a <= ord '9' = (ord a - ord '0') : toIntegerList as 
                     |otherwise                                    = (10 + (ord a) - (ord 'a')) :  toIntegerList as

--De functie fromBase gaat nu weer op dezelfde manier te werk als fromBin en fromDec
fromBase :: Int -> [Char] -> Int
fromBase n s = foldr op 0 (reverse (toIntegerList s))
              where op a b = a + n*b

--Opgave 6
--Ik definieer een hulpfunctie toCharacterList, die van een lijst Intgers een String maakt
toCharacterList :: [Int] -> [Char]
toCharacterList [] = []
toCharacterList (a:as) | a<10 = chr(a + ord '0') : toCharacterList as
                       | otherwise = chr(ord 'a' + a - 10) : toCharacterList as

--toBase gaat weer op dezelfde manier te werk als toDec en toBin
toBase :: Int -> Int -> [Char]
toBase n getal = toCharacterList(reverse (toBaseReverse getal))
                   where toBaseReverse m = if m < n then [m] else (m-x*n) : toBaseReverse x
                                                               where x = div m n

--Opgave 7
-- Eerst controleert de functie mogelijk of elke woord mogelijk is, de niet-mogelijke woorden worden er vervolgens uitgefilterd.
numbers :: Int -> [String] -> [(String, Int)]
numbers n s = map (\x->(x,fromBase n x)) (filter (mogelijk n) s)
                where mogelijk :: Int -> String -> Bool
                      mogelijk _ [] = True
                      mogelijk m (a:as) = (ord a <= m - 10 + ord 'a') && mogelijk m as

--Opgave 8
-- modd rekent het volgende grey-getal uit. hiervoor is een lijst nodig (de 'm-lijst') die bij houdt hoeveel keer een cijfer veranderd is
modd :: Int -> ([Int],[Int]) -> ([Int], [Int]) --eerste van het tuple is de nieuwe 'm-lijst', tweede is je gemodificeerde nummer
modd n ((m:ms), (a:as)) | m>0 = ((m-1):ms, (change a):as)
                        | otherwise = ((n-1):x, a:y)
                         where change b = if b<(n-1) then b+ 1 else 0
                               (x,y) = modd n (ms, as)
modd _ (_, _) = ([], [])


--togray rekent voor één getal de gray codering (als lijst) uit.
togray :: Int -> Int -> [Int]
togray n x = reverse  (nextgray x (m, a))
             where lengte = (length . (toBase n)) x
                   m = ((take lengte) . repeat) (n-1)
                   a = ((take lengte) . repeat) (0)
                   nextgray 0 (_, aa) = aa
                   nextgray xx (mm, aa) = nextgray (xx-1) (modd n (mm,aa))

--fromgray zoekt de juiste integer bij een gray codering (als lijst)
fromgray :: Int -> [Int] -> Int
fromgray n y = find 0 (m,a)
                where lengte = length y
                      m = ((take lengte) . repeat) (n-1)
                      a = ((take lengte) . repeat) (0)
                      ry = reverse y
                      find x (mm,aa) | ry == aa = x 
                                     | otherwise = find (x+1) (modd n (mm,aa))

grayCode :: Int -> ([Char] -> Int, Int -> [Char])
grayCode n = (f,g)
              where f s = fromgray n (toIntegerList s)
                    g x = toCharacterList( togray n x)

-- Opgave 9
sameAndRest :: ([Int], [Int]) -> ([Int],[Int])
sameAndRest (a, []) = (a, [])
sameAndRest (a:as, r:rs) | r==a = sameAndRest(r:a:as, rs)
                         | otherwise = (a:as, r:rs)


groupByValue :: [Int] -> [[Int]]                -- groupByValue deelt een lijst integers op in groepjes met dezelfde waarde
groupByValue [] = []
groupByValue (a:as) =   takeWhile (not . null) (drop 1   (map fst (iterate f ([],a:as) ) ) )       -- f maakt een nieuw groepje
                           where f :: ([Int],[Int]) -> ([Int],[Int])
                                 f (_, []) = ([],[])
                                 f (_, b:bs) = sameAndRest ([b],bs)

nextLookAndSay :: [Int] -> [Int]
nextLookAndSay = concat . map f . groupByValue
                     where f :: [Int] -> [Int]
                           f (y:ys) = length (y:ys) : [y]
                           f [] = []

lookAndSay :: Int -> [String]
lookAndSay x = ((map toCharacterList) . iterate nextLookAndSay) [x]


--Opgave 10
--De reeks is een fibonacci-achtige reeks, dus we gebruiken een soortgelijke methode om die te vinden.
reeks :: [Int] -> [Int]                     
reeks x = x ++ foldr (zipWith (+)) (repeat 0)  (take n (iterate tail (reeks x)))
             where n = length x


--isEenKeithGetal gaat na of een getal in zijn bijbehorende reeks voorkomt. Dit wordt gedaan door de functie check
isEenKeithGetal :: Int -> Bool
isEenKeithGetal x = check x ((reeks . toDec) x) 
                     where check :: Int -> [Int] -> Bool
                           check _ [] = False
                           check n (a:as) | a < n = check n as   -- Check of het getal zelf in de reeks voorkomt
                                          | a == n = True
                                          | otherwise = False
keithGetallen :: [Int]
keithGetallen = filter isEenKeithGetal [10..]