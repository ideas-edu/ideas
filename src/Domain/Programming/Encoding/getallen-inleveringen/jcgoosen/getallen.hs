import Char
import List
 
fromDec :: [Int] -> Int
fromDec x = vermenigvuldigmetmacht ((sum (map turntoone x))-1) x  -- door alle entries te tellen weet ik tot welke macht ik moet gaan

vermenigvuldigmetmacht _ [] = 0
vermenigvuldigmetmacht val (x:xs) = (x*(10^val)) + vermenigvuldigmetmacht (val-1) xs -- door de gegeven val telkens met een te verlagen krijg ik steeds de goede macht om het getal mee te vermenigvuldigen

turntoone x = 1 -- verander alle entries in een..

toDec :: Int -> [Int]
toDec 0 = []
toDec x = (toDec ( (x-(x `mod` 10))`div` 10 )) ++ [(x `mod` 10)] -- door middel van modulo haal ik telkens het laatste getal van de lijst en die concateneer ik dan weer mooi

fromBin::[Int]->Int
fromBin s = fromBinMetHulp s 0

fromBinMetHulp [] hulp = hulp
fromBinMetHulp (x:xs) hulp = fromBinMetHulp xs ((hulp*2) + (x))

toBin::Int->[Int]
toBin b = reverse (toBasehlp b 2)

toBinhlp 0 = []
toBinhlp b = (b `mod` 2) : toBinhlp (b `div` 2)

toBasehlp 0 bas = []
toBasehlp inp bas = (inp `mod` bas ) : toBasehlp (inp `div` bas) bas

toBase::Int->Int->[Char]
toBase bas inp = reverse (map (\x -> if x>9 then  Char.chr ((x)+87) else intToDigit x) (toBasehlp inp bas))

fromBase::Int->[Char]->Int
fromBase bas lst = fromBasehlp bas (reverse lst)

fromBasehlp bas [] = 0
fromBasehlp bas (s:tr) = if ((sb>47) && (sb <58)) then (if ((sb-48)<bas) then ((sb-48)+(bas*(fromBasehlp bas tr))) else -1) else (if(sb<(87+bas)) then ((sb-87)+(bas*(fromBasehlp bas tr))) else -1 )
	 where sb = Char.ord s
	 
	 
	 
numbers::Int->[String]->[(String,Int)]
numbers base lijst = if (base>1)&&(base<37) then (fst (partition (\(str,innt) -> if innt>=0 then True else False) (map (\elmnt -> if (fromBase base elmnt)>=0 then (elmnt,(fromBase base elmnt)) else (""::String,-1)) lijst))) else [("het grondgetal moet 2 t/m 36 zijn, dus niet:",base)]

water::Int->(Int->Int,Int->String)	     
water s = ((s+),show) 

vuur (x,s) = (x 2,s 2)

toGray base num = (toGrayhlp 0 base num)

toGrayhlp pos base num = if ((base^(pos))<num) then ((toGrayhlp (pos+1) base num)++(show (eenGrayPos pos base num)))  else (show (eenGrayPos pos base num))

eenGrayPos pos base num = if val<base then val else (((base*2)-1)-num) 
	   where val = ((num `div` (base ^ (pos)) ) `mod` (base *2))  
	   
--lookAndSay::Int->[String]

keithGetallen::[Int]
keithGetallen =(keithHH 10)


loshakken [] lst = lst
loshakken (inn:inp) lst = loshakken inp (reverse ((Char.digitToInt inn):(reverse lst)))


keithHH n= zz ++ (keithHH (n+1))
	where zz = (filter (==n) ((keithHulp n (length (loshakken (toBase 10 n) [])) (loshakken (toBase 10 n) []))))
	      --where nn=loshakken (toBase 10  n) []

keithHulp inp inplen ding= if ((head(reverse ding))<inp) then (keithHulp inp inplen (reverse (uitpoet:(reverse ding)))) else (reverse (uitpoet:(reverse ding)))
	 where uitpoet=(keithHulpHulp inplen ding) 	    
	  
keithHulpHulp lng lst = sum (take lng (reverse lst))