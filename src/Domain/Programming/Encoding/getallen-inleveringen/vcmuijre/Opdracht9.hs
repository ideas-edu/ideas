--opdracht 9 Valentijn Muijrers 3275183 FP Helium 1.6
module Opdracht9 where
import Char

intNaarChar :: Int -> Char {-Hulp functie die een Integer omzet naar een Character-}
intNaarChar a = if a>=0 && a<10 then chr (a +48) else error " voer een getal in tussen de 0 en 9"
                
lookAndSay :: Int -> [String]
lookAndSay a = [intNaarChar a] : lookAndSayHulp [intNaarChar a] 1 [] 

lookAndSayHulp :: String -> Int -> String -> [String]
lookAndSayHulp [] _ next = lookAndSayNote next 
lookAndSayHulp [b] aantal next = lookAndSayNote (next ++ [intNaarChar aantal] ++ [b]) 
lookAndSayHulp (b:bs:bss) aantal next = if b == bs then lookAndSayHulp (b :bss) (aantal+1) next else lookAndSayHulp (bs:bss) 1 (next ++ [intNaarChar aantal] ++ [b] )

lookAndSayNote :: String -> [String]
lookAndSayNote a = a : lookAndSayHulp a 1 []

{- de hulpfunctie (lookAndSayNote) bepaalt het volgende element in de lijst en stuurt deze door naar de Note functie die hem toevoegt aan de oneindige lijst. De functie werkt voor de getallen van 0 t/m 9 -}