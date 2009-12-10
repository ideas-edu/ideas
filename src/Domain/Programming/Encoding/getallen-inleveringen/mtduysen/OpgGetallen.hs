{-
Practicum opdracht  : Getallen (1)
Auteur              : Martijn Duysens (Student Nr. 3404420)
Compiler            : Helium (1.6)
Version             : Beta version 0.4
-}

module OpgGetallen where

{- Opgave 1
-Functie: 
Geeft van een lijst decimale getallen de intrepetatie als som van 10-machten.
-Werking:
1.Plukt element van de kop van de lijst.
2.Deze wordt vermenigvuldigt met: Bijhorende 10-macht wordt bepaald door de lengte van de rest van de lijst.
3.Dit wordt opgeteld bij het eventuele volgende element uit de lijst door recursieve aanroep van de functie met de tail als parameter.
4.Recursief, dus naar stap 1.
-}
fromDec ::  [Int] -> Int
fromDec []      = 0
fromDec (x: xs) = x * 10 ^ length xs + fromDec xs

{- Opgave 2
-Functie: 
Geef de decimale cijferreeks (in een lijst) van een gegeven getal.
-Werking:
1.Bereken het laatste cijfer van een getal met lastNum (teruggave remainder bij deling door 10) en maak hier een lijst van.
2.De recursieve aanroep van de eigen functie met het laatste getal verwijderd van de parameter(deling door 10, zie nextNum).
3.Recursief, dus naar stap 1.
-}
toDec :: Int -> [Int]
toDec 0       = []
toDec x       = toDec (nextNum x) ++ [lastNum x]
                where  lastNum :: Int -> Int
                       lastNum y = y `rem` 10  
                       nextNum :: Int -> Int
                       nextNum z = z `div` 10

{- Opgave 3
-Functie: 
Geef voor een reeks binaire cijfers de interpretatie als som van 2-machten.
-Werking:
1.Plukt element van de kop van de lijst.
2.Deze wordt vermenigvuldigt met: Bijhorende 2-macht wordt bepaald door de lengte van de rest van de lijst.
3.Dit wordt opgeteld bij het eventuele volgende element uit de lijst door recursieve aanroep van de functie met de tail als parameter.
4.Recursief, dus naar stap 1.
-}
fromBin :: [Int] -> Int
fromBin []      = 0
fromBin (x:xs)  = x * 2 ^ length xs + fromBin xs
                                            

{- Opgave 4
-Functie: 
Geef de binaire cijferijks(in lijstvorm) van een getal.
-Werking:
1.Bereken de remainder bij deling door 2 van het gegeven cijfer en voeg toe aan een lijst.
2.Bereken tevens het overgebleven getal zonder rest bij deling door 2 en geef deze mee als parameter aan de recursieve (eigen) functie.
3.Recursief, dus naar stap 1.
-}
toBin :: Int -> [Int]
toBin 0       = []
toBin x       = toBin (deel x) ++ [remain x]
                where remain :: Int -> Int
                      remain y = y `rem` 2
                      deel :: Int -> Int
                      deel z = z `div` 2                              

{- Opgave 5
-Functie: 
Geef voor een gegeven grondtal en cijferreeks de geintreperteerde waarde
-Werking:
BETA
-}     
fromBase :: Int -> [Int] -> Int
fromBase y []       = 0
fromBase y (x:xs)   = x * y ^ length xs + fromBase y xs 
