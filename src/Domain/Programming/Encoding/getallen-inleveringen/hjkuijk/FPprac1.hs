--Functioneel Programmeren practicum 1
--Door Harm-Joost Van Kuijk (3042901)
{-
  overigens dit is in kladblok geschreven 
  en het schijnt dat als die uitgelezen word in
  notepad++ dat er rare newlines verschijnen door
  rare hexcode 0D en 0A. Het schijnt dat er steeds 
  mac-stijl newlines (ASCII 0D, \r) in het bestand 
  verschijnen. Windows gebruikt 0D 0A (\r\n) en kladblok 
  herkent het teken 0D dus niet. Hint echter wel, dus kunnen 
  er een aantal newlines in het bestand staan die ik niet 
  eens kan weghalen, maar waardoor er dus wel syntaxfouten optreden.
  ik heb de opdrachten op mijn eigen en de uni comp getest en alle 
  opdrachten muv opg 8 doen wat in de opdracht staat.
  dus er bestaan geen fouten..
-}

import Char

--Opgave 1

fromDec :: [Int] -> Int
fromDec [] = 0
fromDec (x:xs) = x * 10 ^ length xs + fromDec xs

--Opgave 2

toDec :: Int -> [Int]
toDec 0 = []
toDec x = toDec (div x 10) ++ mod x 10 : []

--Opgave 3

fromBin :: [Int] -> Int
fromBin [] = 0
fromBin (x:xs) = x * 2 ^ length xs + fromBin xs

--Opgave 4

toBin :: Int -> [Int]
toBin 0 = []
toBin x = toBin (div x 2) ++ mod x 2 : []

--Opgave 5

fromBase :: Int -> [Char] -> Int
fromBase _ [] = 0
fromBase y (x:xs) = naarInt x * (y ^ length xs) + fromBase y xs
           where naarInt :: Char -> Int
                 naarInt z |ord z > 80 = ord z - 87
                           |otherwise = ord z - 48

--Opgave 6
toBase :: Int -> Int -> [Char]
toBase _ 0 = []
toBase y x = toBase y ( div x y ) ++ [f ( mod x y )]
          where f :: Int -> Char
                f z |z > 9 = chr (z + 87)
                    |otherwise = chr (z + 48)


--Opgave 7

numbers :: Int -> [String] ->[(String,Int)]
numbers _ [] = []
numbers y (x:xs)|validCheck y (x, fromBase y x) = (x, fromBase y x) : numbers y xs
                |otherwise = numbers y xs


validCheck :: Int -> (String, Int) -> Bool
validCheck _ ([],_) = True
validCheck y (x:xs,z)|ord x > 80 && ord x - 87 < y = validCheck y (xs,z)
                     |ord x < 80 && ord x - 48 < y = validCheck y (xs,z)
                     |otherwise = False
 


--Opgave 8 NOG TE DEBUGGEN --- lukt niet:(

--grayCode :: Int -> ([Char] -> Int , Int -> [Char])
--grayCode n = (n, (gray 0 n (ntallig n)) 

alfabet :: String
alfabet = ['0'..'9'] ++ ['a'..'z']

ntallig :: Int -> String
ntallig n = take n alfabet

revn :: Int -> String -> [String]
revn 0 _ = []
revn n str = reverse str : revn (n-1) (reverse str)

knip :: Int -> [[a]] -> [[[a]]]
knip _ [] = []
knip 0 xs = [xs]
knip y (x:xs) = (f y x) : (knip y xs)
                        where f :: Int -> [a] -> [[a]]
                              f _ [] = []
                              f n zs = ((take n zs):[]) ++ f n (drop n zs)

--moet altijd aangeroepen worden met (gray 0 n(->als ntallig sys) l(->als init van het ntallig sys)
--met hoever moet er een knip op de 'hoever'ste lijst worden uitgevoerd zodat de juiste lengte string
gray :: Int -> Int -> String -> [String]
gray hoever n l = l : (gray (hoever+1) n (concat f))
              where f = ziprev g h
                          where g = tail (ntallig n)
                                h = revn (n-1) l


{-
grayanders :: Int -> String -> [String]
grayanders n l = l : grayanders n (concat f)
              where f = ziprev g h
                          where g = tail (ntallig n)
                                h = revn (n-1) l
-}


ziprev :: [a] -> [[a]] -> [[a]]
ziprev [] xs = xs
ziprev (y:ys) [] = [y:ys]
ziprev (y:ys) (x:xs) = (concatMap (\e -> [y,e]) x):(ziprev ys xs)


--Opgave 9

lookAndSay :: Int -> [String]
lookAndSay x = las (x:[])

las :: [Int] -> [String]
las l = (l2str l) : las (cfc l)

l2str :: [Int] -> String
l2str [] = ""
l2str (x:xs) = (chr (x + 48)) : l2str xs

cfc :: [Int] -> [Int]
cfc [] = []
cfc (x:xs) = (length(takeWhile (x==) xs) + 1) : [x]
 ++ cfc (dropWhile (x==) xs)



--Opgave 10

sort :: [Int] -> [Int]
sort [] = []
sort (x:[]) = x : []
sort (x:y:xs)|x > y = y : sort (x:xs)
             |otherwise = x : sort (y:xs)


keithCheck :: Int -> Bool
keithCheck x = keith(keithLijst (toDec x)) x

keith :: [Int] -> Int -> Bool
keith [] _ = False
keith (x:xs) y|x < y = keith xs y
              |x == y = True
              |otherwise = False

keithLijst :: [Int] -> [Int]
keithLijst xs = f : keithLijst (drop 1 (xs ++ [f]))
               where f :: Int
                     f = sum(xs)


keithGetallen :: [Int]
keithGetallen = filter keithCheck [10..]

take5keithGetallen :: [Int]
take5keithGetallen = take 5 (keithGetallen)

