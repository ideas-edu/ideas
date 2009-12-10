{--
getallen.hs
Gemaakt door
Naam                : Jalmer van de berg
cs account          : jhberg
studentennummer     : 3048063
Gedraaid en getest op ghc-6.10.1
--}

--Benodigde modules
import Char
import List
import Maybe




--Opdracht 1
--fromDec

--Helft elk cijfer tot de juiste 10-macht en telt de rest erbij op
fromDec :: [Int] -> Int
fromDec [] = 0
fromDec (x:xs) = 10 ^(length xs) * x + (fromDec xs)


--Opdracht 2
--toDec

--Splitst telkens het laatste cijfer af met x `mod` 10 en de rest met x `div` 10
--en plakt de decimale representatie van de rest van het getal voor het laatste cijfer
toDec :: Int -> [Int]
toDec 0 = [0]
toDec x = (toDec (x `div` 10)) ++ [x `mod` 10]




--Opdracht 3
--fromBin

--Werkt net zoals fromDec, alleen dan met 2-machten
fromBin :: [Int] -> Int
fromBin [] = 0
fromBin (x:xs) = 2^(length xs) * x + (fromBin xs)


--Opdracht 4
--toBin

--Werkt net zoals toDec, alleen dan met x `mod` 2 en x `div` 2
toBin :: Int -> [Int]
toBin 0 = [0]
toBin x = (toBin (x `div` 2)) ++ [x `mod` 2]




--Opdracht 5
--fromBase

--Een generalisatie van fromDec en fromBin waarbij een base-macht gebruik wordt
--ook is er een convert functie voor cijfers groter dan 16
fromBase :: Int -> [Char] -> Int
fromBase base [] = 0
fromBase base (x:xs) = base^(length xs) * (convert x) + (fromBase base xs)
                       where convert a | isDigit a = digitToInt a
                                       | otherwise = 10 + ((ord (toLower x)) - 97)


--Opdracht 6
--toBase

--Een generalisatie van toDec en toBin waarbij x `mod` base en x `div` base wordt gebruikt
--Ook is er weer een convert functie
toBase :: Int -> Int -> [Char]
toBase base 0 = "0"
toBase base x = (toBase base (x `div` base)) ++ [convert (x `mod` base)]
                where convert c | c < 10    = intToDigit c
                                | otherwise = chr((c-10) + 97)



--Opdracht 7
--Gray Codes

--levert fromGray en toGray op, toegepast op base
grayCode :: Int -> ([Char] -> Int, Int -> [Char])
grayCode base = (fromGray base, toGray base)


--Produceert de lijst van coderingen van de juiste lengte en zoekt de index op van de gegeven code
--Dit is het getal dat gecodeerd is
fromGray :: Int -> [Char] -> Int
fromGray base code = fromMaybe (-1) (findIndex ((==) code) (reflect base (length code)))


--Produceert de lijst van coderingen van de juiste lengte en levert
--het x-de element op, wat de gray codering voor x is
toGray :: Int -> Int -> [Char]
toGray base x = (reflect base (length (toBase base  x))) !! x


--Roept een recursief algoritme aan dat bij reflect' verder staat toegelicht
reflect :: Int -> Int -> [[Char]]
reflect base n = reflect' base n 0


--Voert het recursieve algoritme uit
--Het basisgeval is als de reeks lengte 1 heeft, dan wordt de lijst van [0..base-1] opgeleverd in stringvorm
--Om een reeks coderingen van lengte n te produceren
--wordt de reeks coderingen van (n-1) base-1 keer gespiegeld
--Vervolgens wordt voor alle strings van deze spiegelingen
--0..base-1 geplakt
--De nieuwe strings die hierdoor opgeleverd worden worden geappend aan de (n-1) reeks coderingen
--Dit levert een nieuwe reeks coderingen van lengte n op
reflect' :: Int -> Int -> Int -> [[Char]]
reflect' base 1 num = map show [0.. (base-1)]
reflect' base n num |  num >= (base-1) = if(num `mod` 2 == 1) then (appendDigit num (reverse prev)) else (appendDigit num prev)
                    | otherwise = if(num `mod` 2 == 1) then ((appendDigit num (reverse prev)) ++ (reflect' base n (num+1)))
                                       else ((appendDigit num prev) ++ (reflect' base n (num+1)))
                          where prev = reflect base (n-1)
                          
--plakt voor elke string in codes het cijfer in character vorm
appendDigit :: Int -> [[Char]] -> [[Char]]
appendDigit digit codes = map (\x -> (head (show digit)) : x) codes



--Opdracht 9
--Look and say reeksen

--Maakt eerst een string van x
--groepeert vervolgens alle elementen in de lijst
--Dus "312211" wordt ["3", "1", "22", "11"]
--Vervolgens mapt hij een functie die van elke sublijst een lijst maakt met daarin de lengte van
--de sublijst omgezet in een character en het eerste element ervan
--Dus ["3", "1", "22", "11"] wordt ["13", "11", "22", "21"]
--concat zorgt er vervolgens voor dat deze sublijsten geconcat worden tot 1 lijst
--Dus ["13", "11", "22", "21"] wordt "13112221"
--iterate past dit telkens toe om de oneindige reeks te produceren

lookAndSay x = iterate (concat . map (\grp -> (head . show)(length grp) : [head grp]) . group) (show x)



--Opdracht 10
--Keith getallen

--Gaat de oneindige lijst van integers af en filtert alle keithGetallen
keithGetallen ::[Int]
keithGetallen = filter isKeithGetal [1..]

--Kijkt of een getal een Keith getal is of niet door te kijken of het groter dan 9
--is en voorkomt in de sequence gegenereert door het getal
isKeithGetal :: Int -> Bool
isKeithGetal x = x > 9 && sortSearch (generateSequence x) x

--Kijkt of een Ord a in een (mogelijk oneindige) gesorteerde lijst zit
sortSearch :: Ord a => [a] -> a -> Bool
sortSearch [] y = False
sortSearch (x:xs) y | x == y = True
                    | x >  y = False
                    | otherwise = sortSearch xs y

--Genereert de sequence die gebruikt wordt om te kijken of een getal een keith getal is
generateSequence :: Int -> [Int]
generateSequence number = (generateSequence' (length digits) digits)
                          where digits = (toDec number)

--Hulpfunctie voor generateSequence die het daadwerkelijke genereren van de sequence doet
--Er is een basisgeval voor het eerste element dat geappend wordt
--De functie roept telkens lastN aan om de cijfers te bepalen die bij elkaar worden
--opgeteld voor het volgende getal
generateSequence' :: Int -> [Int] -> [Int]
generateSequence' len numbers | ((length numbers) == len) = newnumbers ++ (generateSequence' len newnumbers)
                              | otherwise                 = newNumber : (generateSequence' len newnumbers)
                              where
                                    newnumbers = numbers ++ [newNumber]
                                    newNumber = sum (lastN len numbers)

--Hulpfunctie voor generateSequence' die de laatste n elementen van een lijst oplevert
lastN :: Int -> [a] -> [a]
lastN n (x:xs) | n == length (x:xs) = (x:xs)
               | otherwise          = lastN n xs