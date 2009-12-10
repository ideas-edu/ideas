-- Deze code is geschreven door Iris Renckens en Rick van de Hoef. Als 
-- compilers zijn zowel Helium als GHC gebruikt.

-- Opgave 1

-- Deze functie maakt van een lijst van
-- van Ints maar een Int.  
fromDec :: [Int] -> Int         
fromDec [] = 0                      
fromDec [a] = a
fromDec (x:xs) = (x*b) + fromDec(xs)
                   where b = 10^(length(xs))

-- Opgave 2

-- Deze functie maakt van een Int een 
-- lijst van Ints. Vb: van 423 naar [4,2,3]

toList :: Int -> [Int]          
toList 0 = []                   
toList a = b
           where b = (c:toList((a-c) `div` 10))
                   where c = a `mod` 10
                
toDec :: Int -> [Int]
toDec a = reverse(toList a)

-- Opgave 3

-- Deze functie zet een lijst die een binaire
-- code voorstelt om naar een natuurlijk getal

fromBin :: [Int] -> Int         
fromBin [] = 0                  
fromBin (x:xs) = (x*b)+ fromBin(xs)
                   where b = 2^(length(xs))

-- Opgave 4

-- Deze functie zet een natuurlijk getal om in een
-- lijst die een binaire code voorstelt, maar in spiegelbeeld
toBlist :: Int -> [Int]         
toBlist 0 = []                  
toBlist a =  b
            where b = (c:toBlist((a-c) `div` 2))
                    where c = a `mod` 2

-- Deze functie spiegelt de lijst, zodat de lijst daadwerkelijk
-- een binaire lijst voorstelt
toBin :: Int -> [Int]
toBin a = reverse(toBlist a)

-- Opgave 5

-- Deze functie zet een getal (met letters) om naar een getal binnen 
-- een x-tallig stelsel
fromBase :: Int -> [Char] -> Int
fromBase d [] = 0
fromBase d (c:cs) | not (lijstGoed d (c:cs)) = error 
                    "String ligt niet binnen bereik"
                  | 2<=d && d<=36 = ((charToInt (c))*d^length(cs)) 
                    + fromBase d cs
		  | otherwise = error "Het grondgetal zit niet in het bereik"

-- Deze functie controleert of de gegeven lijst van Chars wel bestaat binnen
-- het gegeven d-tallig stelsel.
lijstGoed :: Int -> [Char] -> Bool
lijstGoed d [] = True
lijstGoed d (a:as) | (charToInt a) <= d = lijstGoed d as
                   | otherwise = False

-- Deze functie maakt van een Char een natuurlijk getal. Dit is nodig
-- voor het kunnen rekenen met bijvoorbeeld het hexadecimale stelsel. 
charToInt :: Char -> Int
charToInt x    |e>86 && e<123 = (e-87)
               |e>47 && e<58 = (e-48)
                  where e = fromEnum(x)

-- Opgave 6

-- Deze functie draait de hierna berekende lijst om
toBase :: Int -> Int -> [Char]
toBase a b | a<2 || a>36 = error "Het grondgetal zit niet in het bereik"
           | otherwise = fromList(reverse(toBase' a b))

-- Deze functie berekent de lijst van Ints door de twee gegeven getallen
toBase' :: Int -> Int -> [Int]
toBase' a 0 = []
toBase' a b = ((b `mod` a) : (toBase' a c))
               where c = ((b - (b `mod` a)) `div` a)

-- Deze functie zet een lijst van Ints om naar een lijst van de 
-- bijbehorende Chars
fromList :: [Int] -> [Char]
fromList [] = []
fromList (x:xs)  |x>9 = toEnum (x+87):fromList(xs)
                 |x<10 = toEnum (x+48):fromList(xs)
                 
--Opgave 7

-- Deze functie laat het eindresultaat zien door de String en het getal samen 
-- als tupel in een lijst te stoppen
numbers :: Int -> [String] -> [(String, Int)]
numbers a [] = []
numbers a (c:cs) | goedWoord a c = (c, listToNum a (stringToInt a c 
                   (goedWoord a c))):numbers a cs
                 | otherwise = numbers a cs

-- Deze functie zet de lijst van Int om naar het te berekenen getal
listToNum :: Int -> [Int] -> Int
listToNum a [] = 0
listToNum a (x:xs) = x*a^(length(xs)) + listToNum a xs

-- Deze functie zet de String om naar een lijst van Ints
stringToInt :: Int -> [Char] -> Bool -> [Int]
stringToInt a [] True = []
stringToInt a b False = []
stringToInt a (x:xs) True  |e>86 && e<(87+a) = (e-87):stringToInt a xs True
                           |e>47 && e<58 = (e-48):stringToInt a xs True
                            where e = fromEnum(x)

-- Deze functie kijkt of alle letters van het woord binnen bereik liggen
goedWoord :: Int -> [Char] -> Bool
goedWoord a [] = True
goedWoord a (x:xs)      |e>86 && e<(87+a) = goedWoord a xs 
                        |e>47 && e<58 = goedWoord a xs 
                        |e<=86 || e>=(87+a)|| e <=47 || e>=58 = False
                         where e = fromEnum(x)
	
	
	

-- Opgave 8

-- Deze functie genereert voor een gegeven grondgetal een tweetal functies,
-- waarvan de ene een Int naar een String met het getal in Gray code 
-- convergeert en de ander het omgekeerde doet
grayCode :: Int -> ([Char] -> Int, Int -> [Char])
grayCode g = (fromGray g, toGray g)

-- Deze functie genereert voor een gegeven grondgetal en een natuurlijk
-- getal een String met het getal in Gray code
toGray :: Int -> Int -> [Char]
toGray g a = toGray' g (hoeveel a g 1) a

-- Deze functie doet eigenlijk hetzelfde als de toGray functie, maar bij deze
-- wordt steeds een Char in Gray code gegenereert, en die Chars worden tot 
-- een lijst van Chars samengevoegd
toGray' :: Int -> Int -> Int -> [Char]
toGray' g l a |l == 0 = [(toEnum( (grayChar g l a) + 
                         (getal (grayChar g l a)))::Char)]
              |otherwise =(toEnum((grayChar g l a) +(getal (grayChar g l a)))::
                           Char) : (toGray' g (l-1) a)

-- Deze functie genereert voor een gegeven grondgetal, kolom en natuurlijk 
-- getal de bijbehorende Char in Gray code. Er wordt gekeken naar of het getal
-- voorkomt in de normale vorm van het patroon in de kolom, of de gespiegelde
grayChar :: Int -> Int -> Int -> Int
grayChar g k a	 | even (a `div` (g^(k+1))) = grayChar' g k a 1
                 | otherwise                = omgekeerdgrayChar g k a 1

-- Deze functie bepaalt voor een getal of het een getal 0-9 betreft, of een
-- getal 10 of hoger (tot en met 36). Dit is nodig voor juist gebruik van de
-- toEnum functie in toGray'
getal :: Int -> Int
getal a | a > 9 = 87
        | otherwise = 48

-- Deze functie bepaalt het hoeveelste getal uit het patroon het gegeven getal
-- is
grayChar' :: Int -> Int -> Int -> Int -> Int
grayChar' g k a n | a `rem` (g^(k+1)) >= ((n* g^(k+1)) `div` g) = 
                    grayChar' g k a (n+1)
		  | otherwise = n-1

-- Deze functie is hetzelfde als de vorige, maar geldt dan voor het gespiegelde
-- patroon
omgekeerdgrayChar :: Int -> Int -> Int -> Int -> Int
omgekeerdgrayChar g k a n | a `rem` (g^(k+1)) >= ((n* g^(k+1)) `div` g) = 
                           omgekeerdgrayChar g k a (n+1)		
                          | otherwise = g-n

-- Deze functie bepaalt uit hoeveel de bij het getal horende Gray code moet
-- bestaan
hoeveel :: Int -> Int -> Int -> Int
hoeveel a g n 	| a == 0 = n-1
		| ((a `div` (g^n)) > 0) = hoeveel a g (n+1)
		| otherwise  = n-1

-- Deze functie bepaalt voor een gegeven grondgetal en Gray code het 
-- bijbehorende natuurlijke getal
fromGray :: Int -> [Char] -> Int
fromGray g [] = error "Er bestaat geen GrayCode van void"
fromGray g (a:as) = zeef (mogelijkeInts g c) g (c-1) (a:as)
                     where c = length (a:as)

-- Deze functie filtert met behulp van zeef' een lijst van mogelijke getallen
-- totdat er nog maar 1 overblijft, het gezochte natuurlijke getal
zeef :: [Int] -> Int -> Int -> [Char] -> Int
zeef [] g k (a:as) = error "Lege mogelijke ints lijst bij functie: zeef"
zeef (x:xs) g k [] = x
zeef [] g k [] = error "Bestaat de code bij het gekozen grondgetal?"
zeef (x:xs) g k (a:as)	| length (x:xs) == 1 = x
                        | otherwise = zeef (zeef' (x:xs) g k b) g (k-1) as
                                      where b = charToInt a

-- De eerder genoemde hulpfunctie. Filtert een lijst door te kijken of de
-- Gray code Char van het gegeven getal in een bepaalde kolom gelijk is
-- aan dat van de getallen in de lijst
zeef' :: [Int] -> Int -> Int -> Int -> [Int]
zeef' [] g k a = []
zeef' (x:xs) g k a | (grayChar g k x) == a = (x : (zeef' xs g k a))
                   | otherwise = zeef' xs g k a

-- Deze functie genereert een lijst met getallen die mogelijk horen
-- bij de gegeven Gray code
mogelijkeInts :: Int-> Int -> [Int]
mogelijkeInts g l = [(g^(l-1))..(g^(l)-1)]

-- Opgave 9

-- Deze functie levert de lijst van Strings op
lookAndSay :: Int -> [String]
lookAndSay a =  map naarString (iterate volgendeLookAndSay (toList a))

-- Deze functie zet de lijst van getallen om in een lijst van Strings
naarString :: [Int] -> [Char]
naarString [] = []
naarString (x:xs) = toEnum (x + 48) : naarString xs

-- Deze functie rekent het volgende getal voor de lookAndsay reeks uit
volgendeLookAndSay :: [Int] -> [Int]
volgendeLookAndSay [] = []
volgendeLookAndSay (x:xs) = [length a + 1,x] ++ volgendeLookAndSay b
                            where (a, b) = span (== x) xs
       
-- Opgave 10

-- Deze functie vat alle lijsten met daarin een keithGetal, of niets,
-- samen tot een lijst		
keithGetallen :: [Int]
keithGetallen = concat (map keithGetal [10..])	

-- Deze functie controleert of een bepaald cijfer een keithGetal is.
-- Zo ja, dan wordt deze in een lijst teruggegeven, zo nee, dan wordt 
-- een lege lijst teruggegeven.
checkCijfer :: Int -> [Int] -> Int -> [Int]
checkCijfer a (x:xs) l	| x > a         = []
                        | x == a        = [x]
                        | otherwise     = checkCijfer a (take l (sum 
                                          (take l (x:xs)) : (x:xs))) l

-- Deze functie heeft weinig meerwaarde, maar op deze manier hoeft
-- toList a niet telkens opnieuw uitgerekend te worden tijdens
-- het controleren van een cijfer, net zoals de lengte (toList a)
keithGetal :: Int -> [Int]
keithGetal a = checkCijfer a (toList a) (length (toList a))