{-
    Guido Loupias, student nr 3220559
    Compiler: GHC 6.8.2
-}

import Data.Char

--opg 1
fromDec :: [Int] -> Int
fromDec []     = 0
fromDec (x:xs) = x*10^length(xs) + fromDec xs

--opg2
toDec :: Int -> [Int]
toDec a | a < 10    = [a]
        | otherwise = toDec (a `div` 10) ++ [a `mod` 10]

--opg3
fromBin :: [Int] -> Int
fromBin []     = 0
fromBin (x:xs) = x*2^length(xs) + fromBin xs

--opg4
toBin :: Int -> [Int]
toBin a | a < 2     = [a]
        | otherwise = toBin (a `div` 2) ++ [a `mod` 2]

--opg5
makeInt :: Char -> Int
makeInt c | isHexDigit c = digitToInt c
          | (c >= 'g' && c <= 'z') || (c >= 'G' && c <= 'Z') = ord c - 87

fromBase :: Int -> [Char] -> Int
fromBase a []     = 0
fromBase a (x:xs) = makeInt x * a^length(xs) + fromBase a xs

--opg6
makeChar :: Int -> Char
makeChar c | c < 16   = intToDigit c
           | otherwise = chr (ord 'a' + c - 10)

toBase :: Int -> Int -> [Char]
toBase b a | a < b           = [makeChar a]
           | otherwise       = toBase b (a `div` b) ++ [makeChar (a `mod` b)]

--opg7
numbers :: Int -> [String] -> [(String, Int)]
numbers b []     = []
numbers b xs     = numbers' b ((map (map makeChar).filter p.map (map makeInt)) xs)
                 where p :: [Int] -> Bool
                       p ys = foldr (\x y -> x < b && y) True ys
                       numbers' :: Int -> [String] -> [(String,Int)]
                       numbers' c ys = foldr (\x y -> (x,fromBase c x) : y) [] ys

--opg8
{-
    Gray functies:
    b = base van het getallensysteem
    i = index van een kolom
    n = natuurlijk getal

    Voor verduidelijking zie onderstaand tabelletje waarbij voor b 2 genomen is:
    n       i=1 i=0
    ---------------
    0       0   0
    1       0   1
    2       1   1
    3       1   0
    enz.
-}

--vind het gray getal in kolom i voor een gegeven n
getGrayDigit :: Int -> Int -> Int -> Int
getGrayDigit b i n | m < pr    = m `div` r
                   | otherwise = (b-1) - (m-pr) `div` r
                   where r  = b^i
                         pr = b*r
                         m  = n `mod` (2*b*r)

--converteer een getal n naar een gray reeks
toGray :: Int -> Int -> [Char]
toGray b n = map makeChar (until p f [])
           where p :: [Int] -> Bool
                 p xs = length xs > floor (logBase (toEnum b)  (toEnum n))
                 f :: [Int] -> [Int]
                 f xs = getGrayDigit b (length xs) n : xs

{-
    elke kolom i in een tabel gray reeksen bestaat uit een herhaald patroon.
    in dat patroon komt elk getal voor in een reeks. Die reeks zit in dat
    patroon 2 keer. getGrayRanges vind gegeven een getal wat moet voor komen
    in kolom i van de gray reeks de twee index-paren waarbinnen dat getal voorkomt.

    Bijv:
    n       i=1 i=0
    ---------------
    0       0   0               getGrayRanges 2 0 0 --> ((0,0),(3,3))
    1       0   1
    2       1   1
    3       1   0
-}
getGrayRanges :: Int -> Int -> Int -> ((Int,Int),(Int,Int))
getGrayRanges b i n | d == e-1  = ((c,f),(c,f))
                    | otherwise = ((c,d),(e,f))
                    where r = b^i
                          c = n*r
                          d = c+r-1
                          e = 2*b*r - c - r
                          f = e+r-1

narrowRange :: Int -> (Int,Int) -> ((Int,Int),(Int,Int)) -> (Int,Int)
narrowRange x (a,b) ((c,d),(e,f)) | d >= a && c <= b = correctRange (a,b) (c,d)
                                  | f >= a && e <= b = correctRange (a,b) (e,f)
                                  | otherwise = narrowRange x (a,b) ((x+c,x+d),(x+e,x+f))
                                  where correctRange :: (Int,Int) -> (Int,Int) -> (Int,Int)
                                        correctRange (g,h) (i,j) = (max g i, min h j)

{-
    fromGray:

    Vindt voor het eerste getal in de gray reeks de ranges met getGrayRanges.
    Doe voor elk volgende getal hetzelfde maar zoek alleen binnen de ranges
    van het vorige getal.
-}
fromGray :: Int -> [Char] -> Int
fromGray b (x:xs) = fst (fst (until p f c))
                  where p :: ((Int,Int),[Int]) -> Bool
                        
                        --als d == e-1 dan is er een duplicaat op positie e
                        p ((d,e),[]) = d == e-1 || d == e
                        p ((d,e),_)  = d == e
                        f :: ((Int,Int),[Int]) -> ((Int,Int),[Int])
                        f (ab,(y:ys)) = (narrowRange (2*b*(b^lys)) ab
                                                     (getGrayRanges b lys y),
                                         ys)
                                      where lys = length ys
                        c = (fst (getGrayRanges b (length xs) (makeInt x)),
                             map makeInt xs)

grayCode :: Int -> ([Char] -> Int,Int -> [Char])
grayCode b = (fromGray b, toGray b)

--opg9
{-
    Pak de eerste n gelijke elementen en de rest van de lijst:
    takeEqual [1,1,1,1,0,2] --> ((4,1),[0,2])
-}
takeEqual :: [Int] -> ((Int,Int),[Int])
takeEqual (x:xs) = ((l+1,x),drop l xs)
                 where l = (length.takeWhile(==x)) xs

{-
    Transformeer een reeks getallen naar pairs met hoe vaak ze achter
    elkaar voorkomen en het getal, a.d.h.v. takeEqual
-}
makePairs :: [Int] -> [(Int,Int)]
makePairs [] = []
makePairs xs = fst txs : makePairs (snd txs)
             where txs = takeEqual xs

lookAndSay' :: [Int] -> [[Int]]
lookAndSay' xs = las : lookAndSay' las
               where las = (concat.map(mklist).makePairs) xs
                     mklist :: (Int,Int) -> [Int]
                     mklist (a,b) = [a,b]

lookAndSay :: Int -> [String]
lookAndSay x = [makeChar x] : map (map makeChar) (lookAndSay' [x])

--opg10
{-
    tailReeks x berekent de reeks getallen en levert het
    eerste getal >= x op
-}
tailReeks :: Int -> Int
tailReeks x = head (snd (until (\(z,y:ys) -> y >= z) f a))
            where f :: (Int,[Int]) -> (Int,[Int])
                  f (z,xs) = (z, take (length r - 1) r)
                           where r = foldr (+) 0 xs : xs
                  a = (x,reverse (toDec x))

keith :: Int -> (Int,Bool)
keith x | x < 10    = (x,False)
        | otherwise = (x,tailReeks x == x)

keithGetallen :: [Int]
keithGetallen = (map fst.filter (\(_,b) -> b).map (keith)) [10..]