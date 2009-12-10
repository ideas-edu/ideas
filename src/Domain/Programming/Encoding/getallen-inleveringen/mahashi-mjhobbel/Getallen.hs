import Char
import Maybe
import List



{- Gemaakt door: Mohamed Hashi, Michael Hobbel
   gecompileerd met GHC tijdens het schrijven-}


--a,b::Int
--c:: Char
--l:: [a]

--Hulpfuncties voor alle andere functies---------

convert:: [Char]->[Int]
convert = map intwaarde 

convert':: [Int]->[Char]
convert'= map charwaarde

charwaarde::Int->Char
charwaarde a     | a<10       = intToDigit a
                 | otherwise  =  chr (a+87) 
				 
intwaarde:: Char->Int
intwaarde  c     | isAlpha c  =  ord(toLower c) - 87
			     | isDigit c  =  ord c - 48
                                								
oplopendeMachten:: Int->[Int]->[Int]
oplopendeMachten a []=  [0]
oplopendeMachten a l =  last l: oplopendeMachten a (init(basis))
				where basis = map (*a) l
				
plekInLijst 0 l = head l
plekInLijst a l = plekInLijst (a-1) (tail l)

vervangInLijst 0 x l= x: tail(l)
vervangInLijst a x l=head l : vervangInLijst (a-1) x (tail l)

haalUitLijst 0 l = head l
haalUitLijst a l = haalUitLijst(a-1) (tail l)

		  
----Van en naar het binaire getallensysteem

toBin::Int->[Int]
fromBin:: [Int]->Int

toBin   b   | div b 2 == 0    = [b]
            | otherwise   = (toBin (div b 2)) ++ [(rem b 2)]

fromBin []   = 0
fromBin l    = sum (oplopendeMachten 2 l)

----Van en naar het decimale getallensysteem

toDec:: Int->[Int]
fromDec:: [Int]->Int

toDec   b       | (div b 10) == 0 = [b] 
                | otherwise   = (toDec (div b 10))++[(rem b 10)]

fromDec []        =  0
fromDec l         =  sum (oplopendeMachten 10 l)

----Van en naar een willekeurig getallensysteem

toBase::Int->Int->[Char]
fromBase:: Int->[Char]->Int

toBase  a b |div b a ==0  = [charwaarde(rem b a)]
			|otherwise    = (toBase a (div b a))++[charwaarde(rem b a)]

fromBase a []=0
fromBase a l = sum(oplopendeMachten a (convert l))

----Deze functie kijkt of er in bepaald getallensysteem een getal correspondeert met een string
----Als die bestaat maakt hij een tupel van die string met het desbetreffende getal

numbers::Int->[String]->[(String,Int)]

numbers a []     = []
numbers a (x:xs) | and(map (<a) (convert x))  = (x,fromBase a x): numbers a xs
                 | otherwise                  =  numbers a xs



-----groteLijst maakt voor een getal, een String en een lijst van Strings en nieuwe lijst van Strings
-----Deze lijst is de eerste lijst van Strings met de Chars van de eerste string op kop van die strings.
-----toGray en fromGray kijken dus slechts wat er staat op een bepaalde plaats of op welke plaats iets staat.

grayCode:: Int->([Char]->Int,Int->[Char])
fromGray::Int->[Char]->Int
toGray::Int->Int->[Char]
groteLijst::Int->[Char]->[[Char]]->[[Char]]
lijstje::[a]->[[a]]->[[a]]
lijstje'::[a]->[[a]]

grayCode a = (fromGray a, toGray a)

fromGray a (x:xs) = fromJust(elemIndex (x:xs) (groteLijst ((length (x:xs))-1) (convert' [0,1..(a-1)]) (lijstje'(convert' [0,1..(a-1)]))))                  

toGray a b  = haalUitLijst b (groteLijst (length(toBase a b)-1) (convert' [0,1..(a-1)]) (lijstje' (convert' [0,1..(a-1)])))

groteLijst 0 (x:xs) (y:ys) = (y:ys)
groteLijst a (x:xs) (y:ys) = groteLijst (a-1) (x:xs) (lijstje (x:xs) (y:ys))

lijstje [] (y:ys)=[]
lijstje (x:xs) (y:ys) = map (x:) (y:ys)++ lijstje xs (reverse (y:ys))           

lijstje' []   = [] 
lijstje' (x:xs) = [x]:lijstje' xs


-----look geeft voor een gegeven lijst van Ints, hoeveel er voorkomen van de ints in de lijst
-----lookAndSay gebruikt look recursief om telkens te 'kijken' wat er is.

lookAndSay:: Int ->[String]
look::[Int]->[Char]

lookAndSay a = look (toDec a): lookAndSay(fromDec (convert (look (toDec a))))

look []     = []
look (x:xs) = (intToDigit(length(filter(==x) (x:xs)))):(intToDigit x: look (filter (/=x) xs))

-----keithGetallen is de hoofdfunctie,
-----hierin wordt gecontroleerd of iets voldoet aan de voorwaardes om een keithgetal te zijn.
-----Het gebruikt keithReeks om telkens een reeks te maken per getal om de predicaten te controleren.

keithGetallen::[Int]
keithGetallen''::[Int]->[Int]
keithReeks::Int->[Int]
fibs'::[Int]->[Int]
fibs::[Int]->[Int]

keithGetallen=keithGetallen'' [10,11..]

keithGetallen'' [] =[]
keithGetallen'' (x:xs) | isJust(hulpfunctie x) = x:keithGetallen'' xs
                       | otherwise             = keithGetallen'' xs
                     where hulpfunctie a = find(==a) ( keithReeks a)

keithReeks a  = takeWhile (<=a) (fibs (toDec a))

fibs  (x:xs)  = (x:xs)++(fibs' (x:xs))

fibs' []      = []
fibs' (x:xs)  = fibssom : (fibs' (drop 1 ((x:xs)++[fibssom])))
             where fibssom=sum(x:xs)