--(c) 2009 Wout Elsinghorst, 3344819
--All rights reserved

--woot@skynet-scout:/storage/home/woot/Haskell-PR1> ghc --version
--The Glorious Glasgow Haskell Compilation System, version 6.10.1


import Char

main :: IO () 
main = return ()

--niets bevattende lijst
zeroList :: [Int]
zeroList = 0:zeroList

--lijst met een aantal 1en
oneList :: [Int]
oneList = 1:oneList

--lijst met alle machten van b
powList :: Int -> [Int]
powList b = pow 0 where pow n = (b^n):(pow (n+1))

--vermenigvuldig alle coefficienten in de lijst met de bijbehorende machten uit de powList
fromBaseNum :: Int -> [Int] -> Int
fromBaseNum b list = sum (zipWith (*) (reverse list) (powList b))

--herschrijf het getal tov de basis van machten van de base
toBaseNum :: Int -> Int -> [Int]
toBaseNum b n = reverse (divbase n)
                where divbase 0 = []
                      divbase n = (n `rem` b) : divbase (n `div` b) --remainder kunnen we gebruiken, rest gaat door

--kijk of 2 getallen onder de functie 'op' in een bepaald domein liggen
checkRange :: Ord b => (a -> b) -> a -> a -> a -> Bool
checkRange op x a b = op x >= op a && op x <= op b

--zet een teken om naar zijn numerieke representatie
charToBase :: Char -> Int
charToBase x | checkRange fromEnum x '0' '9' = fromEnum x - fromEnum '0'
             | checkRange fromEnum x 'A' 'Z' = fromEnum x - fromEnum 'A' + 10

--doe het omgekeerde, maar dan zonder toEnum (foutje bedankt)
baseToChar :: Int -> Int
baseToChar v | checkRange id v  0  9  = v + fromEnum '0'
             | checkRange id v 10 36  = v + fromEnum 'A' - 10

--zet misschien een getal tov een bepaalde base in string vorm om naar een getal, of anders, niets
fromBaseStr :: Int -> [Char] -> Maybe Int
fromBaseStr b str | and (map (<b) base) = Just (fromBaseNum b base) -- alle chars in de string zijn < base
                  | otherwise = Nothing
                    where base = map (charToBase.toUpper) str --zet string om naar hoofdletter vorm

--zet eerst om naar een vorm in base-b en converteer daarna naar een string
toBaseStr :: Int -> Int -> [Char]
toBaseStr b n = map (toEnum.baseToChar) (toBaseNum b n)

-- 1)
fromDec :: [Int] -> Int
fromDec list = fromBaseNum 10 list

-- 2)
toDec :: Int -> [Int]
toDec n = toBaseNum 10 n

-- 3)
fromBin :: [Int] -> Int
fromBin list = fromBaseNum 2 list

-- 4)
toBin :: Int -> [Int]
toBin n = toBaseNum 2 n

-- 5)
fromBase :: Int -> [Char] -> Int
fromBase b str = a where (Just a) = fromBaseStr b str

-- 6)
toBase :: Int -> Int -> [Char]
toBase b n = toBaseStr b n

-- 7)
numbers :: Int -> [String] -> [(String, Int)]
numbers b list = transform list
                 where transform [] = []
                       transform (x:xs) = (test (fromBaseStr b x) ) ++ transform xs
                                          where test (Just v) = [(x, v)] --het bevatte alleen letters < b (geldig)
                                                test Nothing = []

--bereken de encoding voor positie p van n in base-b
grayMatter :: Int -> Int -> Int -> Int
grayMatter b n p | even (n `div` b^(p+1) ) = ( (n `div` b^p) `rem` b) --de p-e positie is b^p periodiek
                 | otherwise = b - 1 -       ( (n `div` b^p) `rem` b) --als n / (b^(p+1) oneven is is b^p gespiegeld, tel dan weer terug

maxValue = fromBaseNum 2 (take 31 oneList) -- 0x7FFFFFFF, grootste signed int
maxBaseEncoding b = toBaseNum b maxValue --grootste getal in base b

--encodeer een Int n naar een string tov base b. Werkt niet voor getallen die in hun encodering de meest significante
--positie != 0 hebben. Dit ivm het voorkomen van overflows

encoder :: Int -> Int -> [Char]
encoder b n = reverse (map (toEnum.baseToChar) encodedBits ) --
              where endlessWire p = (grayMatter b n p) : endlessWire (p+1) --hoest encoded bitjes op ad infinitum
                    encodedBits = take (length (maxBaseEncoding b) - 1) (endlessWire 0) 

--niet bepaald efficient... encoder^-1 was leuker geweest
decoder :: Int -> [Char] -> Int
decoder b str = wizard 0
                where kryptonite = map toUpper str --zet str in de uppercase-normal-form
                      wizard n | stripZorro (encoder b n) == stripZorro kryptonite = n --codeer alle n en kijk of de geven str hetzelfde is
                               | otherwise                                         = wizard (n+1)
                      stripZorro [] = []
                      stripZorro (x:xs) | x == '0'    = stripZorro xs --strip alle nulletjes
                                        | otherwise = x:xs

-- 8)
grayCode :: Int -> ([Char] -> Int, Int -> [Char])
grayCode b = (decoder b, encoder b)


--deel een string op in substrings met allemaal dezelfde tekens
splitGroup :: [Char] -> [[Char]]
splitGroup [] = []
splitGroup g = fst (bananaSplit) : splitGroup (snd bananaSplit)
               where bananaSplit = span (\x -> x == (head g)) g

--encodeer een groep: voeg lengte toe aan letter van groep
encodeGroup :: [Char] -> [Char]
encodeGroup g = ( toEnum (baseToChar (length g)) ) : [head g] 

--splits de string in groepen van dezelfde letters, encodeer ze door de lengte van de groep + de bepalende 
--letter achter elkaar te plakken, om ze vervolgens allemaal weer samen te voegen
writeAndSay :: Int -> Int -> [Char]
writeAndSay b n = concat (map encodeGroup (splitGroup (toBaseStr b n) ) )

readAndSay :: Int -> [Char] -> [Char]
readAndSay b str = concat (map encodeGroup (splitGroup str ) )

lookAndScream :: Int -> Int -> [[Char]]
lookAndScream b n = (toBaseStr b n) : loop (writeAndSay b n) --voeg begin waarde toe en reken dan steeds verder met resultaat
                    where loop x = x:loop (readAndSay b x)

-- 9)
lookAndSay :: Int -> [String]
lookAndSay n = lookAndScream 10 n

--genereer een oneindige rij met recurrente betrekkingen in de vorm van R(n) = R(n-1) + R(n-2) .. + R(n-l) met l het aantal decimalen
generate :: Integer -> [Integer]
generate n = base ++ ( degenerate (length base) (generate n) )
             where base = map toInteger (toDec (fromInteger n) )
                   degenerate 1 y = y
                   degenerate x y = zipWith (+) y (degenerate (x-1) (tail y)) --tel (length base - 1) keer de naar links verschoven rij bij de rij op

-- 10)
keithGetallen :: [Integer]
keithGetallen = findThem 10
                where findThem n = check findIt ++ findThem (n+1)
                                   where check (x:_) | x == n    = [n] --als x == n hebben we een keith getal
                                                     | otherwise = [] --zoniet, jammer
                                         findIt = dropWhile (\x -> x < n) (generate n) --ga door tot x >= n


