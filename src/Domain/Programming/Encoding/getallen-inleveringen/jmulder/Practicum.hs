module Practicum where

-- Practicum opdracht van:
--  Jasper Mulder 3363120

--        Compiler:
--          Helium

fromDec        :: [Int] -> Int
fromDec []     =  0
fromDec (x:xs) =  x * 10^(length xs) + fromDec xs

toDec       :: Int -> [Int]
toDec a     =  reverse(toDecSub a)
    where toDecSub 0     = []
          toDecSub b     = b `rem` 10 : toDecSub (b `div` 10)
          
fromBin        :: [Int] -> Int
fromBin []     =  0
fromBin (x:xs) =  x * 2^(length xs) + fromBin xs

toBin       :: Int -> [Int]
toBin a     =  reverse(toBinSub a)
    where toBinSub 0     = []
          toBinSub b     = b `rem` 2 : toBinSub (b `div` 2)
          
fromBase          :: Int -> [Char] -> Int
fromBase a []     =  0 * a
fromBase a (x:xs) |  a>=2 && a<=36 = toNum x * a^(length xs) + fromBase a xs
                  |  otherwise     = error "Deze basis is niet weer te geven"
    where toNum n   | primOrd n >=65 = primOrd n - primOrd 'a' + 10
                    | otherwise      = primOrd n - primOrd '0'

toBase        :: Int -> Int -> [Char]
toBase a 0    =  "0"
toBase a b    |  a>=2 && a<=36 = reverse(map toString (toBaseSub b))
              |  otherwise     = error "Deze basis is niet weer te geven"
    where toBaseSub 0     = []
          toBaseSub c     = c `rem` a : toBaseSub (c `div` a)
          toString d      | d < 10      = primChr( d + primOrd '0')
                          | otherwise   = primChr( d + primOrd 'a' - 10)
                          
numbers      :: Int -> [String] -> [(String, Int)]
numbers a b  =  map (\q -> (q, fromBase a q)) (filter (\r -> and( map (\s -> (toNum s) < a) r)) b)
    where   toNum n   | primOrd n >=65 = primOrd n - primOrd 'a' + 10
                      | otherwise      = primOrd n - primOrd '0'
                      
grayChar     :: Int -> Int -> [Char]
grayChar a 0 =  "0"
grayChar a b |  a>=2 && a<=36 = concatMap (\k -> func k) r
             |  otherwise     = error "Deze basis is niet weer te geven"
    where   func i  = toBase a (min ((b`div`(a^(i-1)))`mod`(2*a)) (((2*a-b-1)`div`(a^(i-1)))`mod`(2*a)))
            r       = takeWhile (\k -> (b >= a^(k))) (iterate (+1) 1)
            
lookAndSay      :: Int -> [[Char]]
lookAndSay a  = iterate lAS (toBase 10 a)
    where   lAS        :: [Char] -> [Char]
            lAS []     = []
            lAS (x:xs) = res ++ lAS (drop y (x:xs))
                where   y   = length(takeWhile (==x) (x:xs))
                        res = (toBase 10 y) ++ [x]
                        
keithGetallen   :: [Int]
keithGetallen   = filter (test) (iterate (+1) 10)
    where test x = last(reeks x)==x
            where   reeks :: Int -> [Int]
                    reeks x = last(takeWhile (\n->(last n<=x)) (iterate (func) (toDec x)))
                        where   func   :: [Int] -> [Int]
                                func a = a ++ [sum(drop (length a-length(toDec x)) a)]
