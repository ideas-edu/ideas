{-
Functioneel Programmeren--------------------------------------------------------
Practicum 1: Getallen

02-03-2009

door Felix Denis
studentnr.: 3350347
-}

module Practicum1 where

--------------------------------------------------------------------------------
--TYPE-DECLARATIES: 
--------------------------------------------------------------------------------
--opgaven:
fromDec::       [Int]   -> Int                                  -- 1
toDec::         Int     -> [Int]                                -- 2
fromBin::       [Int]   -> Int                                  -- 3
toBin::         Int     -> [Int]                                -- 4
fromBase::      Int     -> [Char]       -> Int                  -- 5
toBase::        Int     -> Int          -> [Char]               -- 6
numbers::       Int     -> [String]     -> [(String, Int)]      -- 7
grayCode::      Int     -> ([Char] -> Int, Int -> [Char])       -- 8
lookAndSay::    Int     -> [String]                             -- 9
keithGetallen:: [Int]                                           --10

--------------------------------------------------------------------------------
--hulpfuncties:

--deze constante string wordt in meerdere functies gebruikt
digitChars::    String
{-conversiefuncties tussen twee representaties van getallen, d.w.z.: 
het getal als string en het getal als lijst van de coëfficiënten van de machten 
van de basis.-}
charsToDigits:: [Char]  -> [Int]
digitsToChars:: [Int]   -> [Char]
{-Dit zijn conversiefuncties tussen een getal en de lijst van coëfficiënten van 
de machten van de opgegeven basis. -}
digitsToInt::   Int     -> [Int]        -> Int
intToDigits::   Int     -> Int          -> [Int]
{-Dit zijn conversiefuncties tussen een cijfer als waarde en een cijfer als 
karakter.-}
charToDigit::   Char    -> Int
digitToChar::   Int     -> Char
{-Dit zijn de functies die een getal respectievelijk encoderen en decoderen in 
een string in Gray-code gegeven een basis. -}
toGray::        Int     -> Int          -> [Char]
fromGray::      Int     -> [Char]       -> Int
{-Deze functie doet het echte werk van toGray en fromGray aangezien deze niet 
zoveel verschillen. -}
code::          Int     -> [Int]        -> (Int -> Int) ->      ((Int -> Int) -> Int -> Int)    -> [Int]
{-fibLikes maakt een lijst analoog aan de fibonacci-reeks maar met willekeurige 
startwaarden. Deze lijst is nodig voor het testen of een getal de 
Keith-eigenschap heeft. -}
fibLikes::      [Int]   -> [Int]
--------------------------------------------------------------------------------
--FUNCTIEDEFINITIES: 
--------------------------------------------------------------------------------
--opgaven:
{-fromDec en fromBin zijn twee gevallen van hetzelfde principe en roepen dus een
andere meer algemene functie aan. Hetzelfde geldt voor toDec en toBin. -}
fromDec         = digitsToInt 10

toDec           = intToDigits 10

fromBin         = digitsToInt 2

toBin           = intToDigits 2

{-fromBase en toBase gebruiken ook nog twee functies die cijferreeksen omzetten
naar Strings en vice versa-}
fromBase b cs   = digitsToInt b (charsToDigits cs)

toBase b        = digitsToChars.(intToDigits b)

{-Voor elk woord wordt nagegaan of elke Char in dat woordt een cijfer in de 
basis voorsteld dmv `elem` op de deellijst van digitChars die precies de cijfers
van de basis bevat. De woorden die door de filter komen worden nog even vertaald
en vervolgens in een tupel gestopt. -}
numbers b ws    = map (\w -> (w, fromBase b w)) (filter (\w -> and(map (\c -> c `elem` (take b digitChars)) w)) ws)

grayCode b      = (fromGray b, toGray b)

{-Met iterate wordt de oneindige lijst gemaakt die begint met de String die het 
getal a representeerd en waarbij elk volgend element de 'look-and-say' van zijn
voorganger is. De eigenlijke lookandsay-functie lns werkt met een accumulerende 
parameter die 'zegt' wat lns in de andere lijst 'ziet'. Hierbij wordt mbv 
pattern matching de laatst 'gezegde' waarde vergeleken met de huidige 'geziene'
waarde. Aan het eind wordt het resultaat nog even gereverset zodat alles weer 
goed op zijn plek komt en weer teruggeconverteerd naar één lange String. -}
lookAndSay a    =       iterate next (toBase 10 a)
                        where next cs   =       lns ((head dgs):1:[]) (tail dgs)
                                                where   lns (x:n:say) (y:look)  | x == y    = lns (x:n+1:say) look
                                                                                | otherwise = lns (y:1:x:n:say) look
                                                        lns say []              = foldr (\i j -> toBase 10 i ++ j) "" (reverse say)
                                                        dgs = charsToDigits cs

{-De oneindige lijst keithGetallen filtert alle getallen vanaf 10 door te testen
of ze in hun door hun decimale cijfers voortgebrachte fibonacci-achtige rij
voorkomen. De test werkt recursief en maakt gebruik van de lazy evaluatie van 
(&&) en (||). Zodra het getal gelijk is aan een element wordt meteen True 
opgeleverd. Anders wordt eerst gekeken of het getal al voorbij is. (Dit kan met 
(<) aangezien de fibonaccirij oplopend is.) Alleen als dat nog niet zo is wordt 
de volgende getest. Aangezien de rij een oplopende rij van natuurlijke getallen 
is zal de test dus met zekerheid aflopen en True of False opleveren. -}
keithGetallen   = filter keithTest [10..]
                  where keithTest n     = test n (keithTestSeq n)
                                          where test m (x:xs)   = (m == x) || ((x < m) && test m xs)
                                                keithTestSeq m  = fibLikes (toDec m)

--------------------------------------------------------------------------------
--algemene hulpfuncties: 

digitChars      = "0123456789abcdefghijklmnopqrstuvwxyz"

charsToDigits   = map charToDigit

digitsToChars   = map digitToChar

{-Met een foldl kan weer snel het getal worden gereconstrueerd dat wordt 
gerepresenteerd door de lijst van cijfers-}
digitsToInt b dgts      = if (null.(filter (\x -> 0>x || x>=b))) dgts && (not.null) dgts        then foldl (\x y -> b*x + y) 0 dgts
                                                                                                else error ("digitsToInt: list must not be empty and list must only include values: 0 - " ++ show(b-1))

{- Door steeds de delen door de basis en de rest in de lijst te stoppen wordt de
lijst opgeleverd met de cijferontwikkeling van een getal in een bepaalde basis. 
Let op: een cijfer in deze lijst is dus nog altijd een Int, geen Char. -}
intToDigits b x | x < 0     =   error "intToDigits: only non-negative values allowed"
                | x == 0    =   [0]
                | otherwise =   f x []
                                where f y dgts  | y == 0     = dgts
                                                | otherwise  = f (y `div` b) ((y `rem` b):dgts)

{- charToDigit loopt de lijst digitChars door tot de juiste char gevonden is en 
houdt ondertussen een teller bij die de positie in de lijst aangeeft. De 
waarde van deze teller wordt opgeleverd. -}
charToDigit c   = f c 0 digitChars
                  where f c' n (x:xs) = if (c' == x)    then n
                                                        else f c' (n+1) xs
                        f c' _ []     = error ("charToDigit: character: '" ++ [c'] ++ "' does not refer to a number, only 0-9 and a-z accepted")

{-digitToChar pakt de n-e char uit de lijst digitChars, die zo gekozen is dat 
dit altijd de juiste char oplevert. -}
digitToChar     = (!!) digitChars

--------------------------------------------------------------------------------
--hulpfuncties voor grayCode: 

{-De input wordt eerst omgezet in de 'digits'-representatie (d.w.z.: [Int]), dan
wordt het geëncodeerd/gedecodeerd en dan omgezet in het outputtype. -}
toGray b n      = digitsToChars (code b (intToDigits b n) id id)

fromGray b g    = digitsToInt b (code b (charsToDigits g) id (flip const))

{-Het en-/decoderen gebeurt als volgt: Op elk cijfer wordt een permutatie (swap 
of id) uitgevoerd en een pariteitstest gedaan. Bij het encoderen wordt de test 
na toepassing van de permutatie uitgevoerd, bij het decoderen ervoor. Als een 
cijfer even is wordt dezelfde permutatie doorgegeven aan het volgende cijfer, 
anders wordt met switch de permutatie geruild. Hierbij wordt begonnen bij het 
grootste cijfer, (bijv. 5 in 5023) die de permutatie id meekrijgt. 
Het effect is hetzelfde als bij elke keer dat een cijfer oneven is alle 
volgende cijfers alvast swappen. Alleen wordt hierbij onnodig vaak geswapt 
aangezien tweemaal swap toepassen hetzelfde is als de identiteit. 

Het feit dat encoderen en decoderen maar op één punt verschillen (nl. de 
volgorde van de test en het toepassen van de permutatie) is afgevangen door 
voor beiden een aanroep te doen van code die als extra parameter een functie 
meekrijgt die gegeven een functie f een nieuwe functie oplevert die f wel of 
niet toepast op een gegeven parameter. (Overigens is het effect van encoderen 
en decoderen hetzelfde voor elke oneven basis, wat duidelijk is als men inziet 
dat voor zo'n basis de swap-operatie de pariteit niet verandert en dus geen 
invloed heeft op de uitkomst van de par-functie.)

swap is een permutatie van de getallen van een basis b waarbij 0 t/m b-1 wordt 
omgedraaid tot b-1 t/m 0. Bij een binaire basis wordt een 0 een 1 en andersom, 
bij een decimale basis wordt een 0 een 9, een 1 een 8, enz.-}
code b (x:xs) p t = p x : code b xs (par p x t) t
                  where par p' x' t'    | even (t' p' x')       = p'
                                        | otherwise             = switch b p'
                                                                  where switch b p''    | p'' b == b    = swap b
                                                                                        | otherwise     = id
                                                                                                          where swap b x''      = b-1 - x''
code _ [] _ _     = []

--------------------------------------------------------------------------------
--hulpfunctie voor keithGetallen: 

{-fibLikes maakt een fibonacci-achtige rij door een lijst van getallen als 
beginwaarden te nemen en elk volgend getal als som van evenzoveel voorgangers te
namen als er beginwaarden zijn. fibLikes roept fibLikes' aan die twee parameters
krijgt: de lijst beginwaarden en een lijst van lijsten waar alvast de hele lijst
fibLikes inzit. fibLikes' zet vervolgens alle beginwaarden op kop van elkaar 
maar zet de laatste beginwaarde op kop van een nieuwe lijst, nl. lijst verkregen
door op de eerste na alle lijsten in de 2e parameter met zipWith (+) te ritsen. 
Deze 2e parameter ts is opgebouwd door voor elke beginwaarde de tail van de 
eerste lijst in ts weer toe te voegen aan ts.-}
fibLikes xs     = fibLikes' xs [fibLikes xs]
                  where fibLikes' (x:xs) ts     = x : fibLikes' xs ((tail (head ts)):ts)
                        fibLikes' [] ts         = foldr (zipWith (+)) (repeat 0) (tail ts)
