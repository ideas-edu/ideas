{-
Ivo van den Berg 3020762
Koen Bonenkamp 3230619

Getest met GHC 6.10.1

Practicum 1 - Getallen
-}

module Getallen where
import Char
import List

-- Functies voor meerdere opgaven:
digitValue :: Char -> Int -- Maak van een character een integer (<10)
digitValue c = ord c - ord '0'

digitChar :: Int -> Char -- Maak van een integer (<10) een character
digitChar n = chr(n + ord '0')

digitLetterValue :: Char -> Int -- Maak van een letter met numerieke waarde een integer (>=10 en <=36)
digitLetterValue l = ord l - 87

digitLetterChar :: Int -> Char -- Maak van een integer (>=10 en <=36) een letter met numerieke waarde
digitLetterChar t = chr(t + 87)


-- OPGAVE 1 --
fromDec :: [Int] -> Int
fromDec []       = 0 -- Als de lijst is afgewerkt, dan som + 0 = som
fromDec (x:xs)   = x * 10^l + fromDec xs -- Werk de lijst recursief af en doe elke keer (getal uit lijst * 10^l) + rest lijst
  where l = length xs   -- waarbij l is de lengte van de lijst


-- OPGAVE 2 --
toDec :: Int -> [Int]
toDec n | n < 10     = [n] -- Als n kleiner is dan 10, geef dan gewoon n
        | otherwise  = toDec (n `div` 10) ++ [n `mod` 10] -- Werk te lijst recursief af met modulo berekening


-- OPGAVE 3 --
fromBin :: [Int] -> Int
fromBin []     = 0 -- Als de lijst is afgewerkt, dan som + 0 = som
fromBin (x:xs) = x * 2^l + fromBin xs -- Werk de lijst recursief af en doe elke keer (getal uit lijst * 2^l) + rest lijst
  where l = length xs -- waarbij l is de lengte van de lijst


-- OPGAVE 4 --
toBin :: Int -> [Int]
toBin n | n < 2     = [n] -- Als n kleiner is dan 2, geef dan gewoon n
        | otherwise  = toBin (n `div` 2) ++ [n `mod` 2] -- Werk te lijst recursief af met modulo berekening


-- OPGAVE 5 --
fromBase :: Int -> [Char] -> Int
fromBase _ []                    = 0 -- Als de lijst is afgewerkt, dan som + 0 = som
fromBase base (x:xs) | isDigit x = digitValue x * n + fromBase base xs -- x is een getal
                     | otherwise = digitLetterValue x * n + fromBase base xs -- x is een letter
  where n = base^length xs


-- OPGAVE 6 --
toBase :: Int -> Int -> [Char]
toBase base n | n < base && n < 10     = [digitChar n] -- Als n kleiner is dan base en dan 10, geef dan integer n als character
              | n < base && n >= 10    = [digitLetterChar n] -- Als n kleiner is dan base en groter dan of gelijk aan 10, geef dan letter n als character
              | n >= base && rest < 10 = toBase base next ++ [digitChar rest] -- rest is een getal, ga recursief verder
              | otherwise              = toBase base next ++ [digitLetterChar rest] -- rest is een letter, ga recursief verder
  where rest = n `mod` base -- Het getal dat in de character lijst wordt geplaatst
        next = n `div` base -- Het getal waarmee de recursie verder gaat


-- OPGAVE 7 --
numbers :: Int -> [String] -> [(String,Int)]
numbers _ []                          = [] -- Als de lijst is afgewerkt, geef dan niets (lege lijst)
numbers base (x:xs) | checkStr base x = (x,fromBase base x):numbers base xs -- Als het woord kan met het grondgetal, geef dan het nummer erbij
                    | otherwise       = numbers base xs -- Als het woord niet kan met het grondgetal, ga dan door met de andere woorden

checkStr :: Int -> [Char] -> Bool -- Kijkt of een letter bij een grondtal kan horen
checkStr _ []                             = True -- De lijst is afgewerkt en alle letters horen bij het grondgetal
checkStr base (x:xs) | base <= 10         = False -- Bij een grondgetal lager of gelijk aan 10 hoort helemaal geen letter
                     | ord x >= 87 + base = False -- Het grondgetal is te klein voor letter x
                     | otherwise          = checkStr base xs -- Het grondgetal is goed voor letter x, ga door met de rest van de lijst.


-- OPGAVE 8 --
grayCode :: Int -> ([Char] -> Int, Int -> [Char])
grayCode base = (fromGray base, toGray base) -- De twee functies: (van Gray code naar natuurlijk getal, van natuurlijk getal naar Gray code)

  -- FromGray:
fromGray :: Int -> [Char] -> Int -- Roep de recursieve functie fromGray' aan met de MSB aan kop en een lijst van characters met en het grondgetal van de Gray code
fromGray base (t:xs) = fromBase2 base (x:fromGray' base xs x) -- Maak van de characters eerst normale code met hetzelfde grondgetal
  where x | isDigit t = digitValue t                          -- als de Gray code en converteer het dan naar een natuurlijk getal
          | otherwise = digitLetterValue t

fromGray' :: Int -> [Char] -> Int -> [Int] -- Ga recursief door de lijst characters heen en zet het om in normale code met hetzelfde grondgetal
fromGray' base (s:xs) tSum = sSum `mod` base:fromGray' base xs sSum -- De som van alle afgewerkte cijfers modulo het grondgetal is de volgende bit
  where y | isDigit s = digitValue s
          | otherwise = digitLetterValue s

        sSum = tSum + y -- De som van alle afgewerkte getallen
fromGray' _ _ _              = [] -- De lijst is afgewerkt, dus we zijn klaar

fromBase2 :: Int -> [Int] -> Int -- Hetzelfde als fromBase, alleen werkt deze met een lijst van integers i.p.v. characters (lagere complexiteit)
fromBase2 _ []        = 0 -- Als de lijst is afgewerkt, dan som + 0 = som
fromBase2 base (x:xs) = x * n + fromBase2 base xs -- bereken de som van de hele lijst m.b.v. het grondgetal
  where n = base^length xs


  -- ToGray:
toGray :: Int -> Int -> [Char] -- Roep de recursieve functie toGray' aan met de MSB aan kop en een lijst van cijfers met hetzelfe grondgetal als de te maken Gray code
toGray base n | x < 10    = digitChar x:toGray' base (x:xs) -- x is een cijfer, die hetzelfde is in normale code en Gray code (most significant bit)
              | otherwise = digitLetterChar x:toGray' base (x:xs) -- x is een letter, die hetzelfde is in normale code en Gray code (most significant bit)
  where (x:xs) = toBase2 base n -- Zet het natuurlijke getal eerst om in cijfers met hetzelfde grondgetal als de te maken Gray code, zodat ze dezelfde lengte hebben

toGray' :: Int -> [Int] -> [Char] -- Ga recursief door de lijst getallen met grondgetal base heen en zet het om in Gray code:
toGray' base (x:y:xs) | x < y && opt1 < 10  = digitChar opt1:toGray' base (y:xs) -- x is een cijfer, en is kleiner dan de volgende bit
                      | x < y && opt1 >= 10 = digitLetterChar opt1:toGray' base (y:xs) -- x is een letter, en is kleiner dan de volgende bit
                      | x > y && opt2 < 10  = digitChar opt2:toGray' base (y:xs) -- x is een cijfer, en is groter dan de volgende bit
                      | x > y && opt2 >= 10 = digitLetterChar opt2:toGray' base (y:xs) -- x is een letter, en is groter dan de volgende bit
                      | otherwise           = digitChar 0:toGray' base (y:xs) -- Twee opeenvolgende bits zijn hetzelfde, dus begin weer bij 0.
  where opt1 = y - x -- Optie 1, als x < y
        opt2 = (y + base) - x -- Optie 2, als x > y
toGray' _ _                                 = [] -- De lijst is afgewerkt, dus we zijn klaar


toBase2 :: Int -> Int -> [Int] -- Hetzelfde als toBase, alleen werkt deze met een lijst van integers i.p.v. characters (lagere complexiteit)
toBase2 base n | n < base  = [n] -- Als n kleiner is dan base geef dan integer n (laatste in de lijst)
               | otherwise = toBase2 base next ++ [rest] -- Loop de lijst recursief af en plak alles aan elkaar
  where rest = n `mod` base -- Het getal dat in de lijst wordt geplaatst
        next = n `div` base -- Het getal waarmee de recursie verder gaat


-- OPGAVE 9 --
lookAndSay :: Int -> [String]
lookAndSay n =  iterate (concat.map (\xs -> digitChar (length xs): take 1 xs).group) (intString n)
             {- Geef een getal en maak van dat getal een string. Daarna itereert de functie over die string.
             De functie neemt een string en maakt aparte strings van dubbelen en zet die strings in een lijst (group).
             Daarna nemen we van elk van die substrings in de lijst (map) de lengte en het hoofd ervan (lambda term).
             Die aparte lijsten met lengtes en hoofden voegen we dan weer samen met concat. -}

intString :: Int -> [Char]
intString = map digitChar.toDec -- Maak van een getal een string


-- OPGAVE 10 --
keithGetallen :: [Integer]
keithGetallen = filter keithCheck [10..] -- Filter alle Keith-getallen uit een oneindige lijst

keithCheck :: Integer -> Bool
keithCheck n = keithCheck' xs (length xs) n -- Roep de recursieve keithCheck' aan met de nodige argumenten
  where xs = reverse (toDecInteger n) -- Maak van het te checken getal een lijst van de getallen waaruit het getal is opgebouwd.

keithCheck' :: [Integer] -> Int -> Integer -> Bool
keithCheck' xs l n     | head xs == n    = True -- Als het aanroep getal in de lijst voorkomt, dan is het een Keith-getal
                       | head xs > n     = False -- Als de getallen groter dan het aanroep getal worden, dan is het geen Keith-getal
                       | otherwise       = keithCheck' ((sum (take l xs)):xs) l n -- Maak een lijst waar het aanroep getal in kan zitten

toDecInteger :: Integer -> [Integer] -- Hetzelfde als toDec, alleen dan met Integers i.p.v. Ints
toDecInteger n | n < 10     = [n] -- Als n kleiner is dan 10, geef dan gewoon n
               | otherwise  = toDecInteger (n `div` 10) ++ [n `mod` 10] -- Werk te lijst recursief af met modulo berekening
