-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  unknown
--
-- Test exercises.
-- 
-----------------------------------------------------------------------------

module Domain.Programming.EncodingExercises where

-- fromBin :: [Int] -> Int

fromBins = [ fromBin, fromBinER, fromBinLet, fromBinEta
           , fromBin1, fromBin2, fromBin3, fromBin4, fromBin5, fromBin6
           ]

fromBin = "fromBin = foldl ((+) . (* 2)) 0"

fromBinER =  "fromBin = f 0\n" -- explicit recursion
          ++ "  where\n" 
          ++ "    f nil []     = nil\n" 
          ++ "    f nil (x:xs) = f (((+) . (*2)) nil x) xs\n"

fromBinLet =  "fromBin = let f nil []     = nil\n" 
           ++ "              f nil (x:xs) = f (((+) . (*2)) nil x) xs\n"
           ++ "          in f 0\n"

fromBinEta = "fromBin = foldl (\\x -> (\\y -> ((+) . (* 2)) y) x) 0"

fromBin1 = "fromBin = foldl (\\x y -> x * 2 + y) 0"

fromBin2 =  "fromBin [] = 0\n"
         ++ "fromBin (x:xs) = x * 2^(length xs) + fromBin xs\n"

-- looks like a foldl1, however, solution is not entirely correct (no case for [])
fromBin3 =  "fromBin [x]      = x\n"
         ++ "fromBin (x:y:ys) = fromBin (x * 2 + y : ys)\n"

fromBin4 =  "fromBin = fromBaseInt 2\n"
         ++ "fromBaseInt base xs = sum $ zipWith (*) bMachten xs\n"
         ++ "  where bMachten = scanr (*) 1 $ take (length xs - 1) $ repeat base\n"

fromBin5 =  "fromBin [] = 0\n"
         ++ "fromBin (x:xs) = (x*(2^length xs)) + fromBin xs\n"

fromBin6 =  "fromBin [] = 0\n"
         ++ "fromBin (h:hs) =h*2^length hs +fromBin hs\n"


-- toDec :: Int -> [Int]

toDecs = [toDec, toDec1, toDec2, toDec3, toDec4, toDec5]

toDec = "toDec 0 = [0]\n"
      ++ "toDec n = to n []\n"
      ++ "  where\n"
      ++ "    to 0 = id\n"
      ++ "    to n = let (n', k) = n `divMod` 10\n"
      ++ "           in  to n' . (k :)\n\n"
      ++ "divMod a b = (a `div` b, a `mod` b)\n"

toDec1 =  "toDec 0 = [0]\n"
       ++ "toDec x = toDec' [] x\n" 
       ++ "      where toDec' t 0 = t\n"
       ++ "            toDec' t y =  toDec' ((y `mod` 10) : t) (y `div` 10)\n"

toDec2 =  "toDec 0 = []\n"
       ++ "toDec num = toDec (num `div` 10) ++ [num `rem` 10]\n"

toDec3 =  "toDec 0 = []\n"
       ++ "toDec x = toDec (x `div` 10) ++ [x `mod` 10]\n"

toDec4 =  "toDec x | x < 10    = [x]\n"
       ++ "        | otherwise = toDec (div x 10) ++ [mod x 10]\n"

toDec5 =  "toBaseInt base n = reverse $ convert n\n"
       ++ "   where convert 0 = []\n"
       ++ "         convert n = mod n base : (convert $ div n base)\n"
       ++ "toDec = toBaseInt 10\n"

toDec6 = "toDec x = map (read.(\\x -> [x])) (show x)" -- does not compile

