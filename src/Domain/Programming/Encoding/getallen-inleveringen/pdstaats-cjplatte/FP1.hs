--Praktikum 1 "Getallen"
--Paul Staats, Cas Plattel
--pdstaats, cjplatte
--3404447, 3344908
--Helium compiler

--1
--om deze functie te laten werken defineren we een hulpfunctie fromDec'. We draaien de kop en staart dmv deze extra functie om en vermenigvuldigen
--dan de nieuwe staart met 10 en voegen daar de opvolgende waarde aan toe. Dit doen we in een loop tot de oorspronkelijke lijst leeg is en dan print
--hij het resultaat uit.
fromDec :: [Int] -> Int
fromDec [] = 0
fromDec (x:xs) = fromDec' xs x
fromDec' :: [Int] -> Int -> Int
fromDec' [] tussen = tussen
fromDec' (x:xs) tussen = fromDec' xs (10*tussen+x)

--2
--met de eerste functie hakken we de input int direct in twee stukken, de afgekapte 'div' 10 waarde, en de modulo van 10 (dus het laatste getal)
--deze twee geven we mee als argumenten aan de volgende functie die in essensie hetzelfde doet met de afgekapte eerste waarde en dit dan loopt.
--de waarden worden allemaal op kop van in een tussenresultaat gezet, en als de loop klaar is levert de lege int het tussenresultaat als resultaat.
toDec :: Int -> [Int]
toDec 0 = []
toDec x = toDec' (x `div` 10) [(x `mod` 10)]
toDec' :: Int -> [Int] -> [Int]
toDec' 0 tussen = tussen
toDec' x tussen = toDec' (x `div` 10) ((x `mod` 10):tussen)

--3
--deze functie en hulpfunctie gebruiken dezelfde switch als bij opdr 1. we draaien door de argumenten andersom mee te geven de lijst om. 
--vervolgens maken we een loop die stopt als de lijst leeg is. De loop doet steeds het tussenresultaat maal 2 en de x voegt hij toe om de oude "1"
--te vervangen in het tussenresultaat. (de 1 waardes schuiven als het ware steeds op, weer een keer maal 2)
fromBin :: [Int] -> Int
fromBin [] = 0
fromBin (x:xs) = fromBin' xs x
fromBin' :: [Int] -> Int -> Int
fromBin' [] tussen = tussen
fromBin' (x:xs) tussen = fromBin' xs (2*tussen+x)

--4
--de beginfunctie hakt de gegeven int direct in tweeën, en geeft hem mee aan een hulpfunctie. deze loopt de waarden op zijn beurt door ze net als 
--de beginfunctie te afkapdelen door 2 en de modulo van 2 te nemen. Dit levert steeds 1 op voor oneven waardes en 0 voor evenwaarden. De loop slaat
--de resultaten op in een tussenresultaat variable, precies in de goede volgorde.
toBin :: Int -> [Int]
toBin 0 = []
toBin x = toBin' (x `div` 2) [(x `mod` 2)]
toBin' :: Int -> [Int] -> [Int]
toBin' 0 tussen = tussen
toBin' x tussen = toBin' (x `div` 2) ((x `mod` 2):tussen)

--5
--voor het laten van deze functie hebben we de functies checkValue en convertChar. Hiermee checken we, en converten we chars naar ints. Naast deze
--twee functies maken we gebruik van de prelude functie fromEnum die chars omzet.
checkValue :: Char -> Bool
checkValue x  =  x >= '0' && x <= '9'
convertChar :: Char -> Int
convertChar x
  | checkValue x         =  fromEnum x - fromEnum '0'
  | x >= 'a' && x <= 'z' =  fromEnum x - fromEnum 'a' + 10
  | x >= 'A' && x <= 'Z' =  fromEnum x - fromEnum 'A' + 10
  | otherwise            =  error "convertChar: This was no digit"

--de fromBase hakt de meegekregen lijst direct in twee stukken waar hij de eerste waarde direct laat converten dmv convertChar. hij geeft vervolgens
--drie argumenten mee aan de fromBase' functie. De fromBase loopt de overgebleven lijst door de convertChar heen en slaat het resultaat op in een
--tussenresultaat (zoals vaker gebeurt in vorige opdr.)
fromBase :: Int -> [Char] -> Int
fromBase _ [] = 0
fromBase y (x:xs) = fromBase' y xs (convertChar(x))
fromBase' :: Int -> [Char] -> Int -> Int
fromBase' _ [] tussen = tussen
fromBase' y (x:xs) tussen = fromBase' y xs (y*tussen+convertChar(x))

--6
--met de convertInt zetten we ints om naar chars, dit doen we op soortgelijke wijze als bij opdr 5 met de fromEnum uit de prelude.
convertInt :: Int -> Char
convertInt x
  | x >= 0  && x <=  9   =  toEnum (fromEnum '0' + x)
  | x >= 10 && x <= 15   =  toEnum (fromEnum 'a' + x - 10)
  | otherwise            =  error "convertInt: This was not an int"

--de eerste waarde die meegegeven wordt aan de toBase functie is het grondgetal, hierdoor 'div' en modulo delen levert de mate in als het ware,
--waarin gerekend wordt. Het grondgetal wordt met de afgekapte deling en de geconverte modulo (de convert vanwege de chars voor 10+) meegegeven
--aan de hulpfunctie. Die loopt de resultaten door en slaat ze op in een tussenresultaat. Als de teller van de deling 0 is stopt de loop en wordt
--het tussenresultaat geprint.
toBase :: Int -> Int -> [Char]
toBase _ 0 = []
toBase y x = toBase' y (x `div` y) [(convertInt(x `mod` y))]
toBase' :: Int -> Int -> [Char] -> [Char]
toBase' _ 0 tussen = tussen
toBase' y x tussen = toBase' y (x `div` y) ((convertInt(x `mod` y)):tussen)

--7
--de insideBase functie checkt of er een char in de lijst zit die groter is dan het grondgetal, als dat zo is returnt het false, en anders true.
--de waardes van de lijst worden geconvert door de convertChar (uit een vorige opdr) om te kunnen checken.
insideBase :: Int -> [Char] -> Bool
insideBase _ [] = True
insideBase x (y:ys) = if ((convertChar y) >= x)
                      then False
                      else insideBase x ys

--de functie numbers checkt eerst met de insideBase functie of een bepaald woord mag meedoen. Als dat zo is wordt het grondgetal en de string 
--meegegeven aan numbers' en als derde argument daarbij een lijst van de eerste char van de string en de fromBase daarvan met het grondgetal.
--als een woord niet meedoet gaat hij verder in de loop en neemt hij het volgende woord.
numbers :: Int -> [String] -> [(String,Int)]
numbers _ [] = []
numbers x (y:ys) = if ((insideBase x y))
                   then numbers' x ys [(y,(fromBase x y))]
                   else numbers x ys
--numbers' loopt de gekregen argumenten met het grondgetal op een soort zelfde wijze als numbers doet en verzamelt daarbij een tussenresultaat.
--het tussenresultaat wordt bij de lege lijst gegeven en gereversed omdat het anders in de verkeerde volgorde staat (we plaatsten steeds de latere
--string op kop van het tussenresultaat)
numbers' :: Int -> [String] -> [(String,Int)] -> [(String,Int)]
numbers' _ [] tussen = reverse(tussen)
numbers' x (y:ys) tussen = if ((insideBase x y))
                           then numbers' x ys ((y,(fromBase x y)):tussen)
                           else numbers' x ys tussen
--8
--uit deze opdr zijn we niet gekomen.

--9
--de meegegeven x wordt geconvert naar een char en er wordt een teller 30 meegegeven (deze waarde is zelf instelbaar)
lookAndSay :: Int -> [String]
lookAndSay x = lookAndSay2 x [[(convertInt(x))]] 30

--lookAndSay2 loopt het hele handeltje, hij verlaagd het tellertje (30 in dit geval) elke loop en bij 0 stopt het process omdat het anders vast 
--loopt, 30 zien wij als hoog genoeg omdat hij daar nooit komt in verband met de max lengte van ints. de lookAndSay2 loopt zichzelf dus, hij 
--slaat het resultaat op in een tussenresultaat. Het echte process laat ie over aan lookAndSay'. 
lookAndSay2 :: Int -> [String] -> Int -> [String]
lookAndSay2 _ tussen 0 = tussen
lookAndSay2 x tussen y = lookAndSay2 (lookAndSay' (toBase 10 x) [0]) (tussen++[(toBase 10 (lookAndSay' (toBase 10 x) [0]))]) (y-1)
                         
--lookAndSay' zet de lijst van chars om naar ints door een loop te doen. Hij verwijdert door drop de hoeveelheid van x'en die gevonden worden die achter elkaar hetzelfde zijn. 
--Daarna zet hij de gevonden hoeveelheid in lookAndSay'' voor de x, zodat je als het ware 41 krijgt(bijvoorbeeld) bij [1,1,1,1]. Hij herhaalt zichzelf tot de lijst waarmee hij begon leeg is.
--Dan heeft hij namelijk alles beschreven.          
lookAndSay' :: [Char] -> [Int] -> Int
lookAndSay' [] tussen = (fromDec tussen)
lookAndSay' (x:xs) tussen = lookAndSay' (drop (lookAndSay'' x xs 0) xs) (tussen++((lookAndSay'' x xs 1):[(convertChar x)]))

--Hier doorloopt hij de lijst die hij binnenkrijgt. Hij checkt welke waarde als eerste op de lijst staat. Daarna kijkt hij of de waarde die als tweede staat hetzelfde is als de waarde die hij doorkreeg.
--Als dat zo is, dan doet hij het aantal (wat je terug wilde) +1 en doorloopt hij hem nog een keer. Zo krijg je dus een Int terug, waar de hoeveelheid van dezelfde getallen achter elkaar in staat.
lookAndSay'' :: Char -> [Char] -> Int -> Int
lookAndSay'' _ [] aantal = aantal
lookAndSay'' x xs aantal = if ((head(xs)) == x)
                           then (lookAndSay'' x (tail(xs)) (aantal+1))
                           else (aantal)

--10
--Bevat de lijst met getallen waar we mee beginnen en een lege lijst, waar uiteindelijk de resultaten in checkKeith opgeslagen zullen worden.
keithGetallen :: [Int]
keithGetallen = checkKeith [10..1073741823] []--Aangeraden om aan de rechterkant een lager getal te nemen, met deze duurt het wel heel erg lang.

--Deze vormt de lijst van keithGetallen door steeds het eerste element van de lijst hierboven door te geven en dan degenen waar hij True terugkrijgt op te slaan en uiteindelijk als antwoord op te leveren.
checkKeith :: [Int] -> [Int] -> [Int]
checkKeith [] tussen = tussen
checkKeith (x:xs) tussen = if ((checkKeith' x (toDec x)) == True)
                           then checkKeith xs (tussen++[x])
                           else checkKeith xs tussen

--Bepaalt of een bepaald getal een Keithgetal is, door te kijken of bij het bij elkaar optellen (mbv checkKeith'') van deze getallen boven het grondtal komt. Zo ja, dan is het geen keithgetal meer (want hij zit niet in de eigen lijst
--), daarna checkt hij of hij precies het basisgetal is, zo ja, dan is het een keithgetal. Als hij niet boven en niet hetzelfde is, zit hij er dus onder en gaan we de iteratie nog een keer herhalen
checkKeith' :: Int -> [Int] -> Bool
checkKeith' 0 _ = True
checkKeith' a (x:xs) = if ((checkKeith'' (x:xs) (toBase 10 a)) > a)
                       then False
                       else 
                          if ((checkKeith'' (x:xs) (toBase 10 a)) == a)
                          then True
                          else checkKeith' a (x:xs++[(checkKeith'' (x:xs) (toBase 10 a))])

--Bepaalt de waarde bij optelling van alle waardes die overblijven als je de lengte van a van de omgekeerde lijst x afhaalt. Dan krijg je dus de waardes wanneer je (bijvoorbeeld) de laatste 2 optelt terug.
checkKeith'' :: [Int] -> [Char] -> Int
checkKeith'' x a = foldr1 (+) (take (length(a)) (reverse x))