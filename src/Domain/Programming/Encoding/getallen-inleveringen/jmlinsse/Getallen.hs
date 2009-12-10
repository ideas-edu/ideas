-- +-------------------------+
-- | Practicum 1:   Getallen |
-- +-------------------------+
-- | Jeroen Linssen  3151204 |
-- +-------------------------+
-- | Compiler:    Helium 1.6 |
-- +-------------------------+

-- +----------+
-- | Opgave 1 |
-- +----------+
-- Mijn eerste aanpak was als volgt:
-- -- fromDec pakt het eerste element in de lijst en vermenigvuldigt dit met de juiste 10-macht,
-- -- welke in dit geval mooi de lengte van de tail van de lijst is. Hierna wordt fromDec recursief
-- -- aangeroepen op de tail.
-- -- Als extraatje hebben we ingebouwd dat een element uit de lijst tussen 0 en 9 (inclusief) moet
-- -- liggen.
{-
fromDec :: [Int] -> Int
fromDec []     = 0
fromDec (x:xs) | x >= 0 && x <= 9 = x * 10^(length xs) + fromDec xs
               | otherwise        = error "Graag getallen van 0 t/m 9 gebruiken"
-}
-- Dit is echter nogal duur, omdat de lengte van de tail keer op keer opnieuw wordt uitgerekend.
-- Dit heb ik opgelost door locaal een functie fromDec' :: [Int] -> Int -> Int te definiëren, die
-- meteen de lengte uitrekent van de oorspronkelijke lijst, waarna ieder element met zijn
-- corresponderende 10-macht wordt vermenigvuldigd.
fromDec :: [Int] -> Int
fromDec x = fromDec' x (length x - 1)
          where fromDec' []     _ = 0
                fromDec' (y:ys) z | y >= 0 && y <= 9 = y * 10^z + fromDec' ys (z-1)
                                  | otherwise        = error "Alstublieft getallen van 0 t/m 9 gebruiken"

-- +----------+
-- | Opgave 2 |
-- +----------+
-- toDec roept de functie toDec' aan omdat reverse anders bij iedere recursieve aanroep alle
-- elementen omdraait.
toDec :: Int -> [Int]
toDec 0 = []
toDec x = toDec (x `quot` 10) ++ [(x `rem` 10)]

-- +----------+
-- | Opgave 3 |
-- +----------+
-- fromBin is compleet analoog met fromDec, met als uitzondering dat 2 als grondtal gebruikt wordt.
fromBin :: [Int] -> Int
fromBin x = fromBin' x (length x - 1)
          where fromBin' []     _ = 0
                fromBin' (y:ys) z | y == 0 || y == 1 = y * 2^z + fromBin' ys (z-1)
                                  | otherwise        = error "Alstublieft binaire getallen gebruiken"

-- +----------+
-- | Opgave 4 |
-- +----------+
-- toBin is compleet analoog met toDec, met als uitzondering dat 2 als grondtal gebruikt wordt.
toBin :: Int -> [Int]
toBin 0 = []
toBin x = toBin (x `quot` 2) ++ [(x `rem` 2)]

-- +----------+
-- | Opgave 5 |
-- +----------+
-- fromBase werkt in principe wederom hetzelfde als fromDec en fromBin, maar is een abstractie van
-- beiden. Een grondtal en een String worden opgegeven, waarna in de locaal gedefinieerde functie
-- fromBase' ten eerste gecheckt wordt het grondtal daadwerkelijk toegepast kan worden, dus of het
-- tussen de 2 en de (10 + 26 letters van het alfabet =) 36 (inclusief) ligt. De functie charToInt
-- zorgt ervoor dat de Characters hun corresponderende Integerwaarden krijgen volgens het opgegeven
-- grondtal.
fromBase :: Int -> [Char] -> Int
fromBase 0 _      = 0
fromBase _ []     = 0
fromBase g s = fromBase' g s (length s - 1)
           where fromBase' 0 _      _ = 0
                 fromBase' _ []     _ = 0
                 fromBase' x (y:ys) z | x >= 2 && x <= 36 && c >= 0 && c <= x = c * x^z + fromBase' x ys (z-1)
                                      | otherwise                             = error "Alstublieft correcte getallen gebruiken"
                                      where c = charToInt y

-- charToInt geeft de Integer-waarde terug van een Character
charToInt :: Char -> Int
charToInt x | x >= '0' && x <= '9' = fromEnum x - 48 -- fromEnum '0' is 48
            | x >= 'a' && x <= 'z' = fromEnum x - 87 -- fromEnum 'a' is 97, maar plus 10 vanwege decimale getallen
            | x >= 'A' && x <= 'Z' = fromEnum x - 55 -- fromEnum 'a' is 65, maar plus 10 vanwege decimale getallen
            | otherwise            = error "Dit is geen geldig karakter"

-- +----------+
-- | Opgave 6 |
-- +----------+
-- toBase is behoorlijk analoog aan toDec en toBin met de wijziging dat er een grondtal mee wordt
-- gegeven en een String (ipv. een lijst Integers) wordt teruggegeven. Daarvoor moet natuurlijk
-- een functie aan worden geroepen die de berekende remainder van een bepaald getal en een grondtal
-- omzet naar een Character, in dit geval de functie intToChar.
toBase :: Int -> Int -> [Char]
toBase _ 0 = []
toBase g x = toBase g (x `quot` g) ++ [intToChar (x `rem` g)]

-- intToChar geeft de bijpassende Character terug van een Integer
intToChar :: Int -> Char
intToChar x | x >=  0 && x <=  9 = toEnum (48 + x)
            | x >= 10 && x <= 35 = toEnum (87 + x)
            | otherwise          = error "Dit is geen geldige integer"

-- +----------+
-- | Opgave 7 |
-- +----------+
-- numbers maakt tupels van de opgegeven Strings met hun waarde, die berekend is via het opgegeven
-- grondtal, mits deze Strings gepermitteerd zijn, wat afgehandeld wordt door de functie
-- checkString. De gemaakte tupels worden in een lijst geconcateneerd.
numbers :: Int -> [String] -> [(String,Int)]
numbers _ []     = []
numbers g (s:ss) | checkString g s = (s, fromBase g s) : numbers g ss
                 | otherwise       = numbers g ss

-- checkString geeft een Boolean terug die aangeeft of een String getallen bevat die niet boven het
-- grondtal uitkomen, dus of de String gepermitteerd is of niet.
checkString :: Int -> [Char] -> Bool
checkString _ []     = True
checkString g (s:ss) | y >= 0 && y <= g = checkString g ss
                     | otherwise        = False
                     where y = charToInt s

-- +----------+
-- | Opgave 8 |
-- +----------+
-- Ik ben helaas niet uit deze opgave gekomen, het was mij niet duidelijk wat er moest gebeuren,
-- zelfs na herhaaldelijke uitleg van practicumassistenten.

-- +----------+
-- | Opgave 9 |
-- +----------+
-- lookAndSay stopt eerst de opgegeven Integer in een lijst, vanwege het feit dat de uitvoer een
-- lijst van Strings moet zijn en anders work niet toegepast zou kunnen worden op zijn eigen
-- resultaat m.b.v. de functie iterate. intToChar wordt telkens over het vorige resultaat gemapt,
-- zodat work ermee aan de slag kan.
lookAndSay :: Int -> [String]
lookAndSay = iterate work . map intToChar . (\x -> [x])

-- work neemt een String, waarna charToInt gemapt wordt over de String en de lijst van Integers
-- waar het getal uit bestaat creëert. Alle opeenvolgende elementen die hetzelfde zijn worden
-- gegroepeerd d.m.v. de functie. De functie encode wordt hierna gemapt op alle elementen van de
-- verkregen lijst, waarna het resultaat hiervan geconcateneerd wordt.
work :: String -> String
work = concat . map encode . group . map charToInt

-- group groepeert alle gelijke, opeenvolgende Integers in een lijst in nieuwe lijsten en stopt al
-- deze lijsten in één lijst.
group :: [Int] -> [[Int]]
group []     = []
group (x:xs) = (x:ys) : group zs
             where ys = takeWhile (==x) xs
                   zs = dropWhile (==x) xs

-- encode zet een lijst Integers om naar een String waarin staat hoeveel Integers van wat voor
-- element er achter elkaar staan.
encode :: [Int] -> String
encode i = intToChar (length i) : [intToChar (head i)]

-- +-----------+
-- | Opgave 10 |
-- +-----------+

-- keithGetallen roept loopKeith aan met 10 als startgetal, omdat Keithgetallen groter moeten zijn
-- dan 9.
keithGetallen :: [Int]
keithGetallen = loopKeith 10

-- loopKeith begint vanaf een gegeven getal te checken welke getallen Keith-getallen zijn door
-- isKeith los te laten op het gegeven getal en daarna loopKeith aan te roepen met het opgegeven
-- getal + 1.
loopKeith :: Int -> [Int]
loopKeith x = isKeith x ++ loopKeith (x + 1)

-- isKeith neemt een getal en geeft het terug als een lijst met dat getal erin (vanwege het feit
-- dat check enkel lijsten terug kan geven) als het daadwerkelijk een Keith-getal is of isKeith
-- geeft een lege lijst terug als het opgegeven getal geen Keith-getal is.
-- Om dit te bewerkstelligen wordt check aangeroepen met k als eerste parameter en als tweede een
-- parameter een lijst die verkregen is door create aan te roepen met de lengte van kToDec en
-- kToDec zelf als parameters, waar kToDec staat voor toDec k, dus k gerepresenteerd als lijst met
-- daarin de Integers waaruit hij is opgebouwd.
isKeith :: Int -> [Int]
isKeith k = check k (create (length kToDec) kToDec)
          where kToDec = toDec k

-- check checkt of een gegeven getal k daadwerkelijk een Keith-getal is, door over alle elementen
-- uit een gegeven lijst te lopen totdat hij het getal tegenkomt - waarna hij het getal teruggeeft
-- als zijnde een Keith-getal - of tot hij een getal tegenkomt wat groter is dan het gevraagde
-- getal, wat erop wijst dat dat getal géén Keith-getal is, gezien het getal hierna nooit meer
-- bereikt zal kunnen worden doordat de reeks klimt.
check :: Int -> [Int] -> [Int]
check _ []     = []
check k (x:xs) | x == k    = [k]
               | x <  k    = check k xs
               | otherwise = []

-- create neemt een getal g, namelijk de lengte van het gewenste creategetal en laat addLast met
-- dat getal en de lijst waar create over gaat erop los. Het resultaat (een nieuwe lijst) hangt hij
-- achteraan de oorspronkelijke lijst, waardoor een lijst ontstaat die niet bepaald praktisch te
-- noemen is, maar uiteindelijk wel het doel dient waarvoor hij gebruikt wordt, het is alleen niet
-- erg zuinig in termen van geheugen en tijd.
create :: Int -> [Int] -> [Int]
create g x = x ++ create g (addLast g x)

-- addLast neemt de laatste g elementen van een lijst x en telt deze bij elkaar op.
addLast :: Int -> [Int] -> [Int]
addLast g x = x ++ [sum (drop ((length x) - g) x)]