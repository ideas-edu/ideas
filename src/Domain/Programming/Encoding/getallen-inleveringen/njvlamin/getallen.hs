--Nico Vlaming
--0487465
--Compiler: ghci (via terminal)

import Char

--Opdracht 1, Getallen

--Opgave 1

fromDec::[Int]->Int
fromDec [] = 0
fromDec (h:t) = (h*(10^length t)) + fromDec t

--fromDec [a] = losOp reverse [a]
--losOp (h:t) = h + losOp map (*10) t

--Opgave 2

toDec::Int->[Int]
toDec a = reverse (dec a)
dec 0 = []
dec a  =a`rem`10 :dec (a`div`10)

--Opgave 3

fromBin::[Int]->Int
fromBin [0] = 0
fromBin [1] = 1
fromBin(h:t) = (h*(2^length t)) + fromBin t

--Opgave 4

toBin::Int->[Int]
toBin a = reverse (bin a)
bin 0 = []
bin a = a`rem`2 : bin (a `div` 2)

--Opgave 5

fromBase::Int ->[Char]->Int
fromBase stelsel [] = 0
fromBase stelsel (h:t) = (toInt h *(stelsel^length t)) + fromBase stelsel t

--Hulpfunctie

toInt:: Char ->Int
toInt a | ord a < 90 = ord a -48
    |otherwise = ord a -87

--stringtoInt:: String ->Int    
--stringtoInt [] = 0
--stringtoInt (h:t) | ord h <90 = (ord h -48) + stringtoInt t
--    |otherwise = (ord h -87) + stringtoInt t

--Opgave 6

toBase:: Int -> Int -> [Char]
toBase stelsel a = reverse (base stelsel a)
base stelsel 0 = []
base stelsel a = toChar(a`rem`stelsel) : base stelsel (a `div` stelsel)

toChar:: Int -> Char
toChar a |a<10 = chr(a + 48)
    |(a>10)&&(a<26) = chr(a + 87)
    |otherwise = '?'
    
--Opgave 7
numbers::Int->[String]->[(String, Int)]
numbers stelsel a = number stelsel a 
number stelsel [] = []
number stelsel (h:t) = (h, fromBase stelsel h) : numbers stelsel t

