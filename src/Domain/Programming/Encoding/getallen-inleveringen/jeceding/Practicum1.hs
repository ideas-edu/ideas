{-
Lid 1 van het (one-man-)team: Naam:               Joep Eding
                              Studentnummer:      3396789
                              E-Mail:             J.E.C.Eding@students.uu.nl
                              CS-Loginnaam:       jeceding
Gebruikte Compiler: Glasgow Haskell Compiler versie 6.10.1
-}



{-
Opdracht 1 - fromDec
De waarde van x wordt bepaald door x te vermenigvuldigen met 10^(lengte van de 
rest van de lijst), op die manier wordt correct onderscheid gemaakt tussen de
eenheden, tientallen, honderdtallen, etc. 

Een aandachtspuntje is wel dat deze functie NIET controleert of de input-lijst
Ints bevat die groter zijn dan 9. Voor deze controle zou je de definitie als
volgt aan kunnen passen:
    fromDec x:xs | or(map (>9) (x:xs) = error "Ongeldige input"
                 | otherwise = < huidige derde regel (achter het =-teken) >
Punt van deze oplossing is weer dat het niet bijzonder efficient is, daar bij
elke recursie opnieuw de input wordt gecontroleerd, terwijl die bij de vorige 
controle al correct is bevonden. Een oplossing voor dit probleem is weer om
een nieuwe functie te schrijven die de input controleert en deze daarna door-
geeft aan fromDec
-}

    fromDec :: [Int] -> Int
    fromDec [] = 0
    fromDec (x:xs) = x * 10 ^ (length xs) + fromDec xs



{-
Opdracht 2 - toDec
Het laatste cijfer wordt steeds bepaald door het restant te nemen van de deling 
van dat getal door 10. Het aldus bepaalde cijfer wordt aan het einde van de 
lijst van cijfers van dat getal gezet.Voor het op-één-na-laatste cijfer wordt 
bovenstaande berekening uitgevoerd met het aantal maal dat 10 in het begingetal 
pas (dus x `div` 10). Het bepalen van dit aantal is een manier om het laatste
cijfer van het getal te laten 'vervallen'.
-}

    toDec :: Int -> [Int]
    toDec 0 = []
    toDec x = toDec(x `div` 10) ++ [x `rem` 10]



{-
Opdracht 3 - fromBin
Zelfde als opdracht 1, echter dan voor 2 i.p.v. voor 10

Evenals bij opdracht 1 wordt ook hier niet gecontroleerd of de input correct is.
-}

    fromBin :: [Int] -> Int
    fromBin [] = 0
    fromBin (x:xs) = x * 2 ^ (length xs) + fromBin xs



{-
Opdracht 4 - toBin
Zelfde als opdracht 2, echter dan voor 2 i.p.v. voor 10
-}

    toBin :: Int -> [Int]
    toBin 0 = []
    toBin x = toBin(x `div` 2) ++ [(x `rem` 2)]



{-
Opdracht 5 - fromBase
Converteert getallen van base b naar decimaal.
De gebruikte methode is hetzelfde als die van opdrachten 1 en 3, met het 
verschil dat i.p.v. 2 of 10 hier b gebruikt wordt en dat x eerst nog door
een functie omgezet moet worden van een karakter (Char) naar een getal (Int),
hiervoor wordt de functie getalWaarde gebuikt.
-}

    fromBase :: Int -> [Char] -> Int
    fromBase _ [] = 0
    fromBase b (x:xs) | (b < 2) || (b > 36)   = error "Ingevoerd grondgetal ligt buiten limieten (min: 2, max: 36)!!"
                      | otherwise             = getalWaarde (charString b) b x * b ^ (length xs) + fromBase b xs

    {-     De functie charString dient voor het samenstellen van een string met daarin
    (op volgorde) alle beschikbare karakters voor het betreffende 
    getallenstelsel(gespecificeerd door b) 
    -}
    charString :: Int -> [Char]
    charString b = take b (['0'..'9'] ++ ['a'..'z'])

    {-
    De functie getalWaarde bepaalt, gegeven de lijst met mogelijke karakters
    (op volgorde van klein naar groot!) en het grondtal, de waarde van het
    karakter dat als derde parameter ingevoerd wordt door uit te zoeken op 
    welke plaats van de lijst het staat met mogelijk karakters het staat.
    Let Op: getalWaarde werkt alleen wanneer de ingevoerde lijst met mogelijke
    tekens strookt met het ingevoerde grondgetal.
    -}
    getalWaarde :: [Char] -> Int -> Char -> Int
    getalWaarde [] _ _ = error "Ingelezen karakter staat niet in de lijst van beschikbare tekens voor dit stelsel"
    getalWaarde (x:xs) b n | n == x = (b - length xs) - 1
                           | otherwise = getalWaarde xs b n



{-
Opgave 6 - toBase
Converteert een getal van decimaal naar base b.
Dit gebeurt op dezelfde manier als in opgaven 2 en 4 behalve dat er hier i.p.v
2 of 10 gebruik wordt gemaakt van b en dat het restant van x/b nog door de 
functie tekenWaarde omgezet wordt in een karakter omdat het risico bestaat dat
dit restant groter is dan 9.
-}

    toBase :: Int -> Int -> [Char]
    toBase _ 0 = []
    toBase b x | b < 2                         = error "Het is niet mogelijk om een getal te converteren naar een minder-dan-2-tallig stelsel!"
               | b > (length (charString b))   = error "Wegens gebrek aan beschikbare tekens is dit stelsel niet mogelijk!"
               | otherwise                     = toBase b (x `div` b) ++ [tekenWaarde b (x `rem` b)]

    {- 
    De functie tekenWaarde geeft het teken terug dat decimaal getal x 
    representeert in het b-tallig stelsel 
    -}
    tekenWaarde :: Int -> Int -> Char
    tekenWaarde b x = (charString b) !! x



{-
Opgave 7 - numbers
De functie numbers controleert allereerst of het opgegeven grondgetal binnen de
limieten valt en controleert vervolgens voor elk van de woorden in de lijst of
alle tekens waaruit het bestaat voorkomen in de reeks beschikbare tekens voor 
het gekozen grondgetal (d.m.v. de functie isNumber) en dus een getal 
representeert in dat getallenstelsel. Elk woord dat een getal representeert
wordt vervolgens in een tupel met z'n decimale waarde opgeslagen in een lijst.
Deze lijst met tupels bestaande uit een woord en zijn corresponderende waarde
is de uitvoer van de functie numbers.
-}

    numbers :: Int -> [String] -> [([Char], Int)]
    numbers _ [] = []
    numbers b (x:xs) | b < 2                        = error "Het is niet mogelijk om woorden te controleren voor een minder-dan-2-tallig stelsel!"
                     | b > (length (charString b))  = error "Wegens gebrek aan beschikbare tekens is dit stelsel niet mogelijk!"
                     | isNumber b x                 = (x, (fromBase b x)):numbers b xs
                     | otherwise                    = numbers b xs

    isNumber :: Int -> [Char] -> Bool
    isNumber _ [] = True
    isNumber b xs | b < 2                         = error "Er bestaan geen minder-dan-2-tallige stelsels!"
                  | b > (length (charString b))   = error "Wegens gebrek aan beschikbare tekens is dit stelsel niet bruikbaar"
                  | otherwise                     = and(map (`elem` (charString b)) xs)



{-
Opgave 8 - Gray-coderingen
-}

    {-
    Voor een gegeven grondgetal b produceert de functie grayCode een tupel met
    als eerste element een functie die een ingevoerde 'grayString' omzet in een
    decimale Int en als tweede element een functie die een ingevoerde decimale
    Int omzet in een 'grayString'.
    grayCode produceert deze twee functies door de functies grayToInt en 
    intToGray partieel te parametriseren, dus door ze alleen het eerste 
    element, namelijk het grondgetal, mee te geven.
    -}
    grayCode :: Int -> ([Char] -> Int, Int -> [Char])
    grayCode b | b < 2 = error "Er bestaan geen minder-dan-tweetallige stelsels"
               | b > (length (charString b)) = error "Wegens gebrek aan beschikbare tekens is dit stelsel niet mogelijk!"
               | otherwise = (grayToInt b, intToGray b)
    
    {-
    De functie grayToInt bepaalt van een grayString in het b-tallig stelsel de
    bijbehorende decimale Int door uit een lijst van alle natuurlijke getallen
    die getallen te verwijderen die onmogelijk overeen kunnen komen met de
    gegeven grayString.
    -}
    grayToInt :: Int -> [Char] -> Int
    grayToInt b ys = stripImpossibleResults [0..] b ys '0'
    
    {-
    stripImpossibleResults verwijdert uit een gegeven lijst Ints die Ints die
    niet overeen kunnen komen met (het restant van) de ingevoerde grayString.
    Wanneer je een lijst samenstelt met alle graycodes voor een bepaald 
    grondgetal, dan zie je dat de tekens ('cijfers') van dat systeem 
    afwisselend in op- en aflopende volgorde staan, verder valt op dat hoe 
    verder naar 'links' in de codes je kijkt hoe vaker een cijfer 'van boven 
    naar beneden herhaald wordt':
        000
        001
        011
        010
        110
        111
        101
        100
    Helemaal aan de linker kant wordt een cijfer verticaal 4 keer herhaald 
    voordat het volgende cijfer in dat stelsel aan bod komt, een plaatsje naar
    rechts is dat nog maar 2 keer (maar in het midden 4 keer omdat de rij daar 
    in omgekeerde volgorde nogmaals herhaald wordt) en helemaal rechts wordt 
    een cijfer maar 1 keer 'herhaald', hoewel het lijkt of dat steeds twee keer
    gebeurt is ook dit omdat aan het einde van de serie beschikbare getallen 
    deze in omgekeerde volgorde herhaald wordt. De 'lijst' die de getallen 
    omvat die op de meest linkse plaats staan wordt nooit omgekeerd nogmaals 
    herhaald. Zowel het omdraaien als het 'verticaal herhalen' van reeksen 
    getallen wordt in deze functie gebruikt om het aantal mogelijke resultaten 
    steeds verder terug te dringen. Bij het eerste teken van de grayString dat
    verwerkt wordt wordt er gekeken naar de waarde van dat teken en de lengte 
    van de rest van de string, aan de hand daarvan wordt bepaald tussen welke 
    waarden de waarde van deze string nog kan liggen. Dit interval wordt uit de
    lijst genomen door eerst de hele lijst, tot aan de maximale waarde van de 
    grayString, te nemen en daar dan alles tot de minimale waarde vanaf te 
    laten vallen. Vervolgens wordt de functie opnieuw toegepast op de 
    (beperktere) lijst die het resultaat was van de eerste keer dat de functie 
    toegepast werd en wordt het eerste teken van de grayString niet meer 
    meegegeven. Wanneer de grayString volledig verwerkt is zal er nog maar 1 
    Int overgebleven zijn in de lijst van beschikbare Ints, dit teken is het 
    resultaat van de formule.
    -}
    stripImpossibleResults :: [Int] -> Int -> [Char] -> Char -> Int
    stripImpossibleResults [] _ _ _ = error "De beschikbare getallen zijn op, maar er is geen oplossing gevonden."
    stripImpossibleResults (x:xs) _ [] _ | xs == []   = x
                                         | otherwise  = error "Na het verwerken van de volledige graystring zijn er meerdere oplossingen overgebleven"
    stripImpossibleResults (x:xs) b (y:ys) i = stripImpossibleResults (drop e u) b ys y
                                                 where e = getalWaarde (charString b) b y * b ^ length ys
                                                       u = take (e+f) z
                                                             where f = b ^ length ys 
                                                                   z | not (even (getalWaarde (charString b) b i)) = reverse (x:xs)
                                                                     | otherwise = (x:xs)

    {-
    intToGray produceert de grayCode die in het gespecificeerde getallenstelsel
    hoort bij de ingevoerde Int.
    -}
    intToGray :: Int -> Int -> [Char]
    intToGray b x = makeGrayString b x n 0
                       where n = determineStringLength b x 0
    
    {-
    Omdat de functie makeGrayString vooraan de grayString moet beginnen maar
    wel moet werken met de lengte van de complete grayString (omdat deze nodig
    is voor het bepalen van het aantal verticale herhalingen en het aantal 
    keren dat de lijst van beschikbare tekens in het betreffende 
    getallenstelsel 'op de kop' aan zichzelf vastgemaakt moet worden.
    -}
    determineStringLength :: Int -> Int -> Int -> Int
    determineStringLength _ 0 _ = 0
    determineStringLength b x i | x < b^i = i
                                | otherwise = determineStringLength b x (i+1)

    {-
    makeGrayString produceert een grayString in het b-tallig stelsel die hoort
    bij de decimale Int x. Bij de eerste aanroep van deze functie krijgt hij de
    lengte (n) van de te produceren grayString mee (die vervolgens bij elke 
    recursieve aanroep met 1 verminderd wordt) evenals een waarde die (i) die
    aangeeft met de hoeveelste recursie de functie bezig is.
    In de functie wordt elk teken van de grayString bepaald door teken nummer x
    (xe (ikste) is niet duidelijk zonder superscript) uit de serie tekens voor
    die 'kolom' te nemen. Dus voor het meest linkse teken van een 3-tekens-
    tellende grayCode wordt de lijst gemaakt van het teken in de meest linkse
    positie ('kolom') van alle grayCodes. Deze lijst is gemakkelijk samen te
    stellen doordat je met n en i kunt bepalen hoe vaak een teken 'verticaal'
    herhaald moet worden en hoe vaak de lijst die je met het 'verticaal 
    herhalen' geproduceerd is op de kop aan zichzelf vastgeknoopt moet worden.
    Elk teken moet namelijk b^(n-1) herhaald worden en de op die manier 
    geproduceerde lijst moet b^i keer op de kop aan zichzelf vastgeknoopt
    worden. (dit gebeurt in de definitie bij 'where ys = stri....'
    -}
    makeGrayString :: Int -> Int -> Int -> Int -> [Char]
    makeGrayString _ _ 0 _ = []
    makeGrayString b x n i = a : makeGrayString b x (n-1) (i+1)
                               where a = ys !! x
                                           where ys = stringReverseListsTogether (repeatElements (charString b) (b^(n-1))) (b^i)

    {-
    stringReverseListsTogether zet lijst xs i keer achter elkaar, steeds
    afwisselend in de oorspronkelijke volgorde en omgekeerd.
    -}
    stringReverseListsTogether :: [a] -> Int -> [a]
    stringReverseListsTogether _ 0 = []
    stringReverseListsTogether xs i = xs ++ stringReverseListsTogether (reverse xs) (i-1)
    
    {-
    repeatElements herhaalt de elementen van een lijst i keer, waardoor
        repeatElements [1,2,3] 3
    het volgende resultaat geeft:
        [1,1,1,2,2,2,3,3,3]
    -}
    repeatElements :: [a] -> Int -> [a]
    repeatElements [] _ = []
    repeatElements (x:xs) i = take i (repeat x) ++ repeatElements xs i



{-
Opgave 9 - Look-and-Say Reeksen
De functie lookAndSay produceert een look-and-say reeks voor startwaarde x. Hij
doet dit door een lijst te maken die begint met de 'uitspraak' van de startwaarde
en dan daar de 'uitspraak' van en zo steeds verder, deze taak neemt de functie
rawLookSay op zich. Vervolgens worden de lijsten van Ints in de lijst die 
rawLooksay als uitvoer geeft allemaal omgezet in Strings (lijsten van karakters)
door de functie intListToString. Op kop van de lijst die zo ontstaat wordt de 
'String-representatie' van de startwaarde geplaatst en daarmee is de lijst van
de look-and-say reeks van startwaarde x af ('af' dient met een korreltje zout
genomen te worden daar het een oneindige lijst betreft).
-}

    lookAndSay :: Int -> [String]
    lookAndSay x = (intListToString (intToList x)) : (map intListToString (rawLookSay (intToList x)))

    {-
    intToList converteert een Int naar een lijst van Ints, waarbij elk cijfer
    een los element van de lijst wordt. 
    Zo is [1,2,3] het resultaat van intToList 123
    -}
    intToList :: Int -> [Int]
    intToList 0 = []
    intToList x = intToList (x `div` 10) ++ [x `rem` 10]

    {-
    intListToString converteert een lijst van eencijferige Ints naar de lijst
    van corresponderende Chars.
    -}
    intListToString :: [Int] -> [Char]
    intListToString [] = []
    intListToString (x:xs) = intToChar x : intListToString xs
    
    intToChar :: Int -> Char
    intToChar x | x == 0 = '0'
                | x == 1 = '1'
                | x == 2 = '2'
                | x == 3 = '3'
                | x == 4 = '4'
                | x == 5 = '5'
                | x == 6 = '6'
                | x == 7 = '7'
                | x == 8 = '8'
                | x == 9 = '9'
                | otherwise = error "intToChar werkt alleen voor eencijferige Ints"


    {-
    rawLookSay converteert een lijst van Ints (de lijstrepresenatie van de 
    beginwaarde) naar een lijst van lijsten van Ints (elke lijst van Ints
    is een lijstrepresentatie van een van de getallen in de look-and-say
    reeks).
    -}
    rawLookSay :: [Int] -> [[Int]]
    rawLookSay [] = rawLookSay [0]
    rawLookSay x = y : rawLookSay (y)
                     where y = say x

    {-
    say spreekt de lijst van Ints die hij als invoer krijgt uit en 
    produceert zo z'n uitvoer. Het 'uitspreken' wordt gedaan door eerst
    een lijst te maken van alle 'spans' in de invoerlijst. Met een 'span'
    wordt hier een reeks van identieke Ints bedoeld. Door het 'scheiden' 
    van de spans wordt [1,1,2,2,2] dus [[1,1],[2,2,2]].
    Vervolgens wordt de functie saySpan over alle spans heen 'gemapt'. De
    functie saySpan 'spreekt een span uit'. De functie saySpan geeft, met
    een span (bijvoorbeeld [2,2,2]) als input, als output een lijst van 2
    Ints lang met als eerste Int de lengte van de span (dus hoeveel 2'en 
    er achter elkaar staan) en als tweede Int de Int waaruit de span was
    opgebouwd. Voor het gegeven voorbeeld zou de output van saySpan dus
    [3,2] zijn.
    Wanneer saySpan over de hele lijst van Spans gemapt wordt blijft er
    dus een lijst over met daarin de uitspraken van elke afzonderlijke 
    span, concatenatie van deze uitspraken levert een lijst-representatie
    op van het volgende element in de look-and-say reeks.
    -}
    say :: [Int] -> [Int]
    say = concat . map saySpan . spans

    spans :: [Int] -> [[Int]]
    spans [] = []
    spans (x:xs) = (x:ys):spans zs
                    where (ys,zs) = span (==x) xs

    saySpan :: [Int] -> [Int]
    saySpan xs = [length xs, head xs]



{-
Opgave 10 - Keithgetallen
-}

    {-
    keithGetallen produceert een (oneindige) lijst van getallen die aan de
    voorwaarden voor een Keithgetal voldoen. Deze lijst wordt geproduceerd
    door een lijst van alle positieve getallen te filteren met isKeithGetal,
    een functie die, gegeven een Int x, True als uitkomst heeft als het 
    ingevoerde getal een Keithgetal is.
    -}
    keithGetallen :: [Int]
    keithGetallen = filter isKeithGetal [1..]

    {-
    isKeithGetal controleert van een gegeven Int of deze onderdeel uitmaakt
    van de Keithlijst waarvan dat getal zelf de startwaarde is.
    De functie controleert natuurlijk niet met de volledige Keithlijst,
    aangezien dat een oneindige lijst is. Daarvoor dient 'take (x `div` 2)',
    dit zorgt ervoor dat slechts de eerste 0.5x elementen van de lijst worden
    genomen. Als het een Keithgetal betreft zit dat getal hier sowieso tussen.
    
    'take (x `div` 2)' kan waarschijnlijk nog wel herschreven worden om een
    kleinere selectie te maken die toch altijd het Keithgetal (als dat er is)
    bevat.
    -}
    isKeithGetal :: Int -> Bool
    isKeithGetal x = elem x lijst && x > 9
                       where lijst = take (x `div` 2) (keithLijst x)

    {-
    keithLijst converteert een ingevoerde Int naar een lijst van Ints met 
    de functie intToList en bepaalt de lengte van deze lijst. Met deze twee
    waarden wordt vervolgens de functie maakKeithLijst aangeroepen, de lijst
    van Ints die geproduceerd wordt door maakKeithLijst is ook de uitvoer van
    keithLijst.
    -}
    keithLijst :: Int -> [Int]
    keithLijst x = maakKeithLijst (intToList x)

    {-
    Gegeven een lijst met Ints zet maakKeithLijst het eerste element op kop van
    de lijst die gevormd wordt door maakKeithLijst nogmaals aan te roepen met
    als 'invoerlijst' z. Waarbij z gevormd wordt door alle elementen van de 
    invoerlijst op te tellen, deze waarde aan het einde van deze lijst te zetten
    en de eerste waarde in de lijst te laten vervallen.
    -}
    maakKeithLijst :: [Int] -> [Int]
    maakKeithLijst [] = error "Geen beginwaarden opgegeven"
    maakKeithLijst (x:xs) = x : maakKeithLijst z
                                where z = xs ++ [sum (x:xs)]