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
-- Student solutions.
-- 
-----------------------------------------------------------------------------

module Domain.Programming.EncodingExercises where

import Common.Strategy as CS
import Domain.Programming.Prog
import Domain.Programming.Strategies


--------------------------------------------------------------------------------
-- Help Functions
--------------------------------------------------------------------------------
incorrect = label "Incorrect" CS.fail

--------------------------------------------------------------------------------
-- Categories: good, good with modification, clumsy, incorrect.
--------------------------------------------------------------------------------
fromBinsGood = [fromBin, fromBin1,fromBin2,fromBin3,fromBin5,fromBin6,fromBin7,fromBin8,fromBin9,fromBin11,fromBin12,fromBin13,fromBin14,fromBin16,fromBin18,fromBin21,fromBin22,fromBin23,fromBin25,fromBin26,fromBin28,fromBin29,fromBin30,fromBin31,fromBin32,fromBin33,fromBin34,fromBin35,fromBin36,fromBin37,fromBin38,fromBin40,fromBin41,fromBin42,fromBin45,fromBin46,fromBin48,fromBin49,fromBin50,fromBin51,fromBin53,fromBin55,fromBin56,fromBin57,fromBin59,fromBin60,fromBin61,fromBin62,fromBin64,fromBin66,fromBin67,fromBin68,fromBin69,fromBin70,fromBin72,fromBin73,fromBin74,fromBin75,fromBin77,fromBin78,fromBin79,fromBin80,fromBin81,fromBin82,fromBin83,fromBin85,fromBin86,fromBin89,fromBin90,fromBin93,fromBin94,fromBin95,fromBin96]

fromBinsGoodModified = [fromBin15', fromBin17', fromBin27']

fromBinsTodo = [ fromBin20, fromBin47, fromBin58, fromBin65
               , fromBin87, fromBin91]

fromBinsClumsy = [ fromBin10, fromBin19, fromBin43, fromBin44, fromBin52, fromBin54
                 , fromBin71, fromBin76, fromBin84, fromBin92
                 ]

fromBinsWrong = [fromBin4, fromBin63, fromBin88]

fromBinsNoCompile = [fromBin24 {- can be fixed by inlining mapAccumR -}]


--------------------------------------------------------------------------------
-- Solutions
--------------------------------------------------------------------------------
-- Stefan his standard solution
fromBin = Solution "fromBin = foldl ((+) . (* 2)) 0"
                   "Stefan"
                   fromBinFoldlS
                   ""

-- Student solutions:
fromBin1 = 
  Solution ("fromBin = fromBase' 2\n"
         ++ "fromBase' _ []     = 0\n"
         ++ "fromBase' n (x:xs) = x * n ^ length xs + fromBase' n xs\n")
            "affboth-lveerman"
            fromBinRecurS
           ("Length calculation not necessary, could be tupled. "
         ++ "The base parameter `n' is invariant.")

fromBin2 = 
  Solution "fromBin = foldl (\\x y -> x * 2 + y) 0"
           "bdoren-mjspoor"
           fromBinFoldlS
           ""

fromBin3 = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin (x:xs) = x * 2^(length xs) + fromBin xs\n")
            "bgreeven-jsteenbe"
            fromBinRecurS
            "Length could be tupled."


fromBin4 = 
  Solution ("fromBin [x]      = x\n"
         ++ "fromBin (x:y:ys) = fromBin (x * 2 + y : ys)\n")
            "bjliefer-ptpkokke"
            incorrect
            "The function is not defined on the empty list"


fromBin5 = 
  Solution ("fromBin = fromBaseInt 2\n"
         ++ "fromBaseInt base xs = sum $ zipWith (*) bMachten xs\n"
         ++ "  where bMachten = scanr (*) 1 $ take (length xs - 1) $ repeat base\n")
            "bspaans"
            fromBinZipWithS
            "Clumsy way of describing iterate with scanr"

fromBin6 = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin (x:xs) = (x*(2^length xs)) + fromBin xs\n")
            "btdijk"
            fromBinRecurS
            ""

fromBin7 = 
  Solution "fromBin = foldl ( (+).(2*) ) 0"
           "cjblom"
           fromBinFoldlS
           ""

fromBin8 = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin (h:hs) =h*2^length hs +fromBin hs\n")
            "ddtoniss-dsgroote"
            fromBinRecurS
            ""

fromBin9 = 
  Solution ("fromBin l = foldr op 0 (reverse l)\n"
         ++ "  where op a b = a + 2*b\n")
            "dekuppev"
            fromBinFoldlS
            ""

fromBin10 = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin x@(y:ys) = y * 2^(length x - 1) + fromBin ys\n")
            "dgerritz"
            fromBinRecurS
            "Clumsy way of calculating the length."

fromBin11 = 
  Solution ("fromBin []       =  0\n"
         ++ "fromBin (x : xs) =  x * 2 ^ (length xs) + fromBin xs\n") 
            "dpboot"
            fromBinRecurS
            ""

fromBin12 = 
  Solution ("fromBin [] = 0\n"
        ++ "fromBin (x:xs) = x * (2^(length xs)) + fromBin xs\n")
           "dwinkel"
           fromBinRecurS
           ""

fromBin13 = 
  Solution ("fromIntBase b = foldl' (\\ x y -> b*x+y ) 0\n"
        ++ "fromBin = fromIntBase 2\n") 
           "elrenkem-ogrottie"
           fromBinFoldlS
           ""

fromBin14 = 
  Solution "fromBin   = foldl (\\r n -> 2 * r + n) 0\n"
           "ergallo"
           fromBinFoldlS
           ""

fromBin15 = 
  Solution ("fromNumeralSystem base input\n"
         ++ "    | and (map (<base) input) = foldl ((+).(base*)) 0 input\n"
         ++ "    | otherwise = error \"Input not within limits of base\"\n"
         ++ "fromBin = fromNumeralSystem 2\n")
            "ewjmulde"
            fromBinFoldlS
            ""
fromBin15' = 
  Solution ("fromNumeralSystem base input = foldl ((+).(base*)) 0 input\n"
         ++ "fromBin = fromNumeralSystem 2\n")
            "ewjmulde"
            fromBinFoldlS
            "Rewritten by hand: removed checks."

fromBin16 = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin (x:xs) |x == 1 = (2^(length xs)) + fromBin xs\n"
         ++ "               |otherwise = fromBin xs\n")
            "fcbijlsm"
            fromBinRecurS
            "This solutions uses a guard instead of a multiplication. Rewrite?"

fromBin17 = 
  Solution ("fromBin = digitsToInt 2\n"
         ++ "digitsToInt b dgts = if (null.(filter (\\x -> 0>x || x>=b))) dgts && (not.null) dgts\n"
         ++ "                     then foldl (\\x y -> b*x + y) 0 dgts\n"
         ++ "                     else error (\"digitsToInt: list must not be empty and list must only include values: 0 - \" ++ show(b-1))") 
            "fldenis"
            fromBinFoldlS
            ""
fromBin17' = 
  Solution ("fromBin = digitsToInt 2\n"
         ++ "digitsToInt b dgts = foldl (\\x y -> b*x + y) 0 dgts\n") 
            "fldenis"
            fromBinFoldlS
            "Rewritten by hand: removed checks."

fromBin18 = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin [s] = s\n"
         ++ "fromBin (s:t:staart) = fromBin ((2*s+t):staart)\n")
            "fsteeg-hkbarnev"
            fromBinRecurS
            "Good solution, might be a model solution."

fromBin19 = 
  Solution ("fromBin  []    = 0\n"
         ++ "fromBin (x:xs) = 2 ^ (length (x:xs) -1 ) * x + fromBin xs")
            "gcpzunde"
            fromBinRecurS
            "Clumsy length calculation."

fromBin20 = 
  Solution ("fromBin = fromBaseI 2\n"
         ++ "fromBaseI base = foldl op 0\n"
         ++ "    where op a b | abs b < base = (abs a * base + abs b) * sign a * sign b\n"
         ++ "                 | otherwise = error \"Invoer cijfers groter dan basis.\"\n"
         ++ "              where sign x | x < 0 = -1\n"
         ++ "                           | otherwise = 1\n") 
            "gdijkstr-rjhensin"
            fromBinFoldlS
            "Unnecessary calculations, like abs and sign."

fromBin21 = 
  Solution ("fromBin []     = 0\n"
         ++ "fromBin (x:xs) = x*2^length(xs) + fromBin xs\n") 
            "gloupias"
            fromBinRecurS
            ""

fromBin22 = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin (x:xs) = x * 2 ^ length xs + fromBin xs\n") 
            "hjkuijk"
            fromBinRecurS
            ""

fromBin23 = 
  Solution ("fromBin = fromBaseIndex 2\n"
         ++ "fromBaseIndex _ [] = 0\n"
         ++ "fromBaseIndex base (i:is) = base^(length is)*i + fromBaseIndex base is\n") 
            "hlversto"
            fromBinRecurS
            ""

fromBin24 = 
  Solution ("fromBin = fromSys 2\n"
         ++ "fromSys s l = foldl (+) 0 (snd (mapAccumR convertNthDigit 0 l))\n"
         ++ "            where convertNthDigit x y | y < s     = (x+1) y*s^x)\n"
         ++ "                                      | otherwise = error \"number is greater then base\"\n") 
            "hmpaasse"
            fromBinFoldlS
            "This solution does not compile in Helium, due to the mapAccumR."

fromBin25 = 
  Solution ("fromBin []     = 0\n"
         ++ "fromBin (x:xs) = x * 2^l + fromBin xs\n"
         ++ "  where l = length xs\n") 
            "imberg-cwbbonen"
            fromBinRecurS
            ""

fromBin26 = 
  Solution ("fromBin s = fromBinMetHulp s 0\n"
         ++ "fromBinMetHulp [] hulp = hulp\n"
         ++ "fromBinMetHulp (x:xs) hulp = fromBinMetHulp xs ((hulp*2) + (x))\n") 
            "jcgoosen"
            fromBinRecurS
            "Another good solutions, similar to fromBin18."

fromBin27 = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin (x:xs) = let y = length xs\n"
         ++ "                 in if (x == 1 || x == 0) then x * 2 ^ y + fromBin xs else error \"niet binair\"\n") 
            "jcgsmits"
            fromBinRecurS
            ""
fromBin27' = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin (x:xs) = let y = length xs\n"
         ++ "                 in x * 2 ^ y + fromBin xs\n") 
            "jcgsmits"
            fromBinRecurS
            ""

fromBin28 = 
  Solution "fromBin = foldl (\\x y -> 2*x+y) 0"
           "jdfeddem"
           fromBinFoldlS
           ""

fromBin29 = 
  Solution ("fromBin [] =  0\n"
         ++ "fromBin (x:xs) =  x * 2 ^ length xs + fromBin xs\n") 
            "jduijn-iduijn"
            fromBinRecurS
            ""

fromBin30 = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin (x:xs) = x * 2 ^ (length xs) + fromBin xs\n") 
            "jeceding"
            fromBinRecurS
            ""

fromBin31 = 
  Solution ("fromBin ints = fromBinHulp 0 (reverse ints)\n"
         ++ "  where fromBinHulp _ [] = 0\n"
         ++ "        fromBinHulp n (i:is) | i < 2     = (i*2^n) + fromBinHulp (n+1) is\n"
         ++ "                             | otherwise = undefined\n") 
            "jgageldo"
            fromBinFoldlS
            "Fine solution, almost a foldr."

fromBin32 = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin (x:xs) = 2^(length xs) * x + (fromBin xs)\n") 
            "jhberg"
            fromBinRecurS
            ""

fromBin33 = 
  Solution ("fromBin = fromBaseN 2\n"
         ++ "fromBaseN base number = fromBaseN' base (reverse number)\n"
         ++ "    where fromBaseN' _ [] = 0\n"
         ++ "          fromBaseN' base' (c:cs) = c + base' * (fromBaseN' base' cs)\n") 
            "jhorn"
            fromBinFoldlS -- foldrS
            ""

fromBin34 = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin (a:b) = let macht = 2^(length (b))\n"
         ++ "                in a * (macht) + fromBin b\n") 
            "jjhoozem"
            fromBinRecurS
            ""

fromBin35 = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin (x:xs) = x * 2 ^ n + fromBin xs\n"
         ++ "  where n = length xs\n") 
            "jkoperdr"
            fromBinRecurS
            ""

fromBin36 = 
  Solution ("fromBin []     = 0\n"
         ++ "fromBin (x:xs) = x*b + fromBin xs\n"
         ++ "  where b = 2^length xs\n") 
            "jleersum-anieuwla"
            fromBinRecurS
            ""

fromBin37 = 
  Solution ("fromBin x = fromBin' x (length x - 1)\n"
         ++ "  where fromBin' []     _ = 0\n"
         ++ "        fromBin' (y:ys) z | y == 0 || y == 1 = y * 2^z + fromBin' ys (z-1)\n"
         ++ "                          | otherwise        = error \"Alstublieft binaire getallen gebruiken\"\n") 
            "jmlinsse"
            fromBinRecurS
            ""

fromBin38 = 
  Solution ("fromBin []     =  0\n"
         ++ "fromBin (x:xs) =  x * 2^(length xs) + fromBin xs\n") 
            "jmulder"
            fromBinRecurS
            ""

fromBin40 = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin (x : xs) = x * 2 ^ (length xs) + fromBin xs\n") 
            "jtkman-echgbon"
            fromBinRecurS
            ""

fromBin41 = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin (x : xs) = x*2^length xs + fromBin xs\n") 
            "jwind"
            fromBinRecurS
            ""

fromBin42 = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin (x:xs) = result + fromBin xs\n"
         ++ "  where result = x * (2 ^ length xs)\n") 
            "kfaro"
            fromBinRecurS
            ""

fromBin43 = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin (x : []) = x\n"
         ++ "fromBin (x : xs) = ( x * 2^length xs ) + fromBin xs\n") 
            "kjdvoors-apol"
            fromBinRecurS
            ""

fromBin44 = 
  Solution ("fromBin (a : as) = a*(2^(length(a : as)-1)) + fromBin(as)\n"
         ++ "fromBin [] = 0\n") 
            "lblhartm"
            fromBinRecurS
            ""

fromBin45 = 
  Solution ("fromBin x = fromBin' x (length x - 1)\n"
         ++ "fromBin' [] _ = 0\n"
         ++ "fromBin' (x:xs) y = x*2^y + (fromBin' xs (y - 1))\n") 
            "ldsbroe-jfklein"
            fromBinRecurS
            ""

fromBin46 = 
  Solution ("fromBin []     = 0\n"
         ++ "fromBin (x:xs) = x * 2^length(xs) + fromBin xs\n") 
            "lrwester"
            fromBinRecurS
            ""

fromBin47 = 
  Solution ("fromBin (x:xs) = x * 2^(length xs) + fromBin xs\n"
        ++ "fromBin []     = 0\n") 
           "lsstoel"
           fromBinRecurS
           ""

fromBin48 = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin xs = fromBin' 0 (reverse xs)\n"
         ++ "fromBin' _ [] = 0\n"
         ++ "fromBin' a (x:xs) = 2^a * (abs x `mod` 2) + fromBin' (a+1) xs\n") 
            "ltbinsbe"
            fromBinFoldlS -- foldrS
            ""

fromBin49 = 
  Solution "fromBin = foldl ((+).(*2)) 0" 
           "lwgraaff"
           fromBinFoldlS
           ""

fromBin50 = 
  Solution ("fromBin []   = 0\n"
         ++ "fromBin l    = sum (oplopendeMachten 2 l)\n"
         ++ "oplopendeMachten a []=  [0]\n"
         ++ "oplopendeMachten a l =  last l: oplopendeMachten a (init(basis))\n"
         ++ "  where basis = map (*a) l\n") 
            "mahashi-mjhobbel"
            fromBinZipWithS
            "Inefficient: gebruik van last en init."

fromBin51 = 
  Solution ("fromBin []     = 0\n"
         ++ "fromBin (x:xs) | x==0 || x==1 = x * 2 ^ (length xs) + (fromBin xs)\n"
         ++ "               | otherwise    = error \"Binaire getallen hebben alleen nullen en enen) dommerd!\"\n") 
            "maooster"
            fromBinRecurS
            ""

fromBin52 = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin [bin] = bin\n"
         ++ "fromBin (bin:rest) = bin * 2^length rest + fromBin rest\n") 
            "mbarendr"
            fromBinRecurS
            ""

fromBin53 = 
  Solution "fromBin = foldl (\\n c -> 2 * n + c) 0"
           "mfrancke-jfidijks"
           fromBinFoldlS
           ""

fromBin54 = 
  Solution ("fromBin x = fromBinRev (reverse x) 0\n"
         ++ "fromBinRev [] _ = 0\n"
         ++ "fromBinRev [x] y  = x * 2^y\n"
         ++ "fromBinRev (x:xs) y = x * 2^y + fromBinRev xs (y+1)\n") 
            "mgrimme"
            fromBinFoldlS -- foldr
            ""

fromBin55 = 
  Solution ("fromBin = fromSpecial 2\n"
         ++ "fromSpecial _ [] = 0\n"
         ++ "fromSpecial base xs | or (map (\\y -> y >= base || y<0) xs) = error \"Please provide a list only containing values in the given range\"\n"
         ++ "                    | otherwise = sum (zipWith (*) (reverse xs) [base^x | x <- [0..] ])\n") 
            "mkroese"
            fromBinZipWithS
            ""

fromBin56 = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin getalLijst = 2 * fromBin (init getalLijst) + last getalLijst\n") 
            "mlmbroer-jmwbrete"
            fromBinRecurS
            ""

fromBin57 = 
  Solution "fromBin = foldl (\\x y -> 2*x + y) 0\n"
           "mmontvai-beck"
           fromBinFoldlS
           ""

fromBin58 = 
  Solution ("fromBin a = from 2 a\n"
         ++ "from a b | (or . (map (a<))) b = error \"[Int]->Int conversion out of range\"\n"
         ++ "         | otherwise         = from'' a b\n"
         ++ "from'' a [] = 0\n"
         ++ "from'' a [b] = b\n"
         ++ "from'' a ( kop : tussen : staart ) = from'' a ([(kop * a) + tussen ] ++ staart)\n") 
            "mrvaarti-rwerken"
            fromBinRecurS
            ""

fromBin59 = 
  Solution ("fromBin []      = 0\n"
         ++ "fromBin (x:xs)  = x * 2 ^ length xs + fromBin xs\n") 
            "mtduysen"
            fromBinRecurS
            ""

fromBin60 = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin xs = fromBim (reverse xs)\n"
         ++ "fromBim [] = 0\n"
         ++ "fromBim (x:xs) | x < 0 || x > 1 = error \"Niet binair getal!\"\n"
         ++ "               | otherwise = x + 2 * fromBim xs\n") 
            "mtibboel-sabitter"
            fromBinFoldlS -- foldr
            ""

fromBin61 = 
  Solution ("fromBin x = maakGetal 2 x 0\n"
         ++ "maakGetal _ [] b = b\n"
         ++ "maakGetal a (x:xs) b = if (x < a) then  maakGetal a xs (b * a + x) else 0\n") 
            "mvensela-merboxel"
            fromBinRecurS
            ""

fromBin62 = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin (x : xs) = x * 2 ^ length xs + fromBin xs\n") 
            "mzwan-jgeeden"
            fromBinRecurS
            ""

fromBin63 = 
  Solution ("fromBin [0] = 0\n"
         ++ "fromBin [1] = 1\n"
         ++ "fromBin(h:t) = (h*(2^length t)) + fromBin t\n") 
            "njvlamin"
            fromBinRecurS
            ""

fromBin64 = 
  Solution "fromBin a = foldl (\\x y->(2*x)+y) 0 a\n"
           "nroumimp"
           fromBinFoldlS
           ""

fromBin65 = 
  Solution ("fromBin = fromAap 2\n"
         ++ "fromAap base = foldl (nextInt base) 0\n"
         ++ "  where nextInt base x y | y >= base = error (concat[\"fromAap: Number \", show y, \" out of range for base \", show base,\".\")\n"
         ++ "                         | y < 0 = error (concat[\"fromAap: Number \", show y, \" is negative.\")\n"
         ++ "                         | otherwise = x * base + y\n") 
            "ompennin"
            fromBinFoldlS
            ""

fromBin66 = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin (x:xs) = fromBin' xs x\n"
         ++ "fromBin' [] tussen = tussen\n"
         ++ "fromBin' (x:xs) tussen = fromBin' xs (2*tussen+x)\n") 
            "pdstaats-cjplatte"
            fromBinFoldlS -- foldr
            ""

fromBin67 = 
  Solution ("fromBin    x     =  toInt 2 x\n"
         ++ "toInt    _      []                           = 0\n"
         ++ "toInt    b      (x:xs)  | x < 0 || x > b - 1 = -1\n"
         ++ "                        | otherwise          = x * (b ^ l) + toInt b xs\n"
         ++ "                          where l = length xs\n") 
            "pjwjanse-jjvisser"
            fromBinRecurS
            ""

fromBin68 = 
  Solution "fromBin xs = foldl' (\\x y -> x*2+y) 0 xs\n"
           "pmspek"
           fromBinFoldlS
           ""

fromBin69 = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin (x:xs) = x*(2^length xs)+fromBin xs\n") 
            "pqgroot"
            fromBinRecurS
            ""

fromBin70 = 
  Solution ("fromBin cs = if isBinair cs then foldl (\\ x y -> x*2 + y) 0 cs else error \"Invoer is niet Binair!\"\n"
         ++ "isBinair [] = True\n"
         ++ "isBinair (c:cs) | c == 1 || c == 0 = isBinair cs\n"
         ++ "                | otherwise = False\n") 
            "raspauwe"
            fromBinFoldlS
            ""

fromBin71 = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin (x:xs)  | x <= 1 = x * (2 ^ (length (x:xs) - 1)) + fromBin xs\n"
         ++ "                | otherwise = error \"lijst mag alleen uit 1 en 0 bestaan!\"\n") 
            "ravries-ccmrooij"
            fromBinRecurS
            ""

fromBin72 = 
  Solution ("fromBin []     = 0\n"
         ++ "fromBin (x:xs) = x * 2^length xs + fromBin xs\n") 
            "rawagenm"
            fromBinRecurS
            ""

fromBin73 = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin (x:xs) = (x*b)+ fromBin(xs)\n"
         ++ "   where b = 2^(length(xs))\n") 
            "rehoef-irrencke"
            fromBinRecurS
            ""

fromBin74 = 
  Solution ("fromBin x = fromIntGet 2 x\n"
         ++ "fromIntGet _ [] = 0\n"
         ++ "fromIntGet g (x:xs) =  g^(length xs) * x + fromIntGet g xs\n") 
            "rgroot"
            fromBinRecurS
            ""

fromBin75 = 
  Solution ("fromBin = fromBaseInt 2\n"
         ++ "fromBaseInt n = (foldr (\\x y -> x + n*y) 0) . reverse\n") 
            "rhaan"
            fromBinFoldlS
            ""

fromBin76 = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin [x] = x\n"
         ++ "fromBin (x:xs)= getal + fromBin xs\n"
         ++ "  where getal = x * (2 ^ length xs)\n") 
            "rhouweli"
            fromBinRecurS
            ""

fromBin77 = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin (c:cs) = c * (2 ^ length cs) + fromBin cs\n") 
            "rpvermeu-jcrvrijho"
            fromBinRecurS
            ""

fromBin78 = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin (x:xs) =   x * 2^length xs + fromBin xs\n") 
            "rslagmol"
            fromBinRecurS
            ""

fromBin79 = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin (x:xs) = x*2^(length xs) + fromBin xs\n") 
            "rtharder"
            fromBinRecurS
            ""

fromBin80 = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin (x:xs) = x * 2 ^ length xs + fromBin xs\n") 
            "rvesten-rsalphen"
            fromBinRecurS
            ""

fromBin81 = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin (x:xs) = x*2^(length xs) + fromBin xs\n") 
            "sagieske"
            fromBinRecurS
            ""

fromBin82 = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin (x:xs) = (x * 2 ^ length (xs))+ fromBin (xs)\n") 
            "sdriel"
            fromBinRecurS
            ""

fromBin83 = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin (x:xs) = x * 2 ^ length xs  + fromBin xs\n") 
            "sjavissc"
            fromBinRecurS
            ""

fromBin84 = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin (x:xs) = x * 2 ^ z + fromBin xs\n"
         ++ "  where z = length(x:xs) - 1\n") 
            "sjveldhu"
            fromBinRecurS
            ""

fromBin85 = 
  Solution "fromBin l = sum $ zipWith (*) (map (2^) [0..(length l)-1]) (reverse l)\n"
           "sttimmer"
           fromBinZipWithS
           ""

fromBin86 = 
  Solution ("fromBin [a] = a\n"
         ++ "fromBin (a:as) = a * (2 ^ (length (a:as) - 1)) + fromBin(as)\n") 
            "talles"
            fromBinRecurS
            ""

fromBin87 = 
  Solution ("fromBin x = doFromBin (reverse x) 0\n"
         ++ "  where doFromBin (y:ys) z = y * 2^z + doFromBin ys (z +1)\n"
         ++ "        doFromBin [] _ = 0\n") 
            "taveld"
            fromBinFoldlS -- foldr
            ""

fromBin88 = 
  Solution ("fromBin x = fromBin2 x ((length x) - 1)\n"
         ++ "fromBin2 (a:[]) _ = a\n"
         ++ "fromBin2 (h:t) x = (h * (2 ^ x)) + fromBin2 t (x - 1)\n") 
            "tleroi"
            fromBinRecurS
            ""

fromBin89 = 
  Solution ("fromBin = listToNumber 2\n"
         ++ "listToNumber _ [] = 0\n"
         ++ "listToNumber stelsel (h:t) = h*(stelsel^(length t)) + (listToNumber stelsel t)\n") 
            "tmsoetho"
            fromBinRecurS
            ""

fromBin90 = 
  Solution ("fromBin ys = fB ys (length ys - 1)\n"
         ++ "  where fB [] _ = 0\n"
         ++ "        fB (x:xs) n = (x * 2^n) + fB xs (n-1)\n") 
            "tromberg-rjanssen"
            fromBinRecurS
            ""

fromBin91 = 
  Solution ("fromBin' (h:[]) _   = h\n"
         ++ "fromBin' (h:t) l = (h * (2 ^ l)) + (fromBin' t (l - 1))\n"
         ++ "fromBin v = fromBin' v ((length v) - 1)\n") 
            "tsteemer"
            fromBinRecurS
            ""

fromBin92 = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin (x:xs) = x * (2 ^ l) + fromBin xs\n"
         ++ "  where l = length (x:xs) -1\n") 
            "vrbons-hckampma"
            fromBinRecurS
            ""

fromBin93 = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin (x:xs) = x * (2 ^ (length xs)) + fromBin xs\n") 
            "whustinx"
            fromBinRecurS
            ""

fromBin94 = 
  Solution ("fromBin list = fromBaseNum 2 list\n"
         ++ "fromBaseNum b list = sum (zipWith (*) (reverse list) (powList b))\n"
         ++ "  where powList b = pow 0 where pow n = (b^n):(pow (n+1))\n") 
            "wlelsing"
            fromBinZipWithS
            ""

fromBin95 = 
  Solution ("fromBin [] = 0\n"
         ++ "fromBin (x:xs) = x*2^y + fromBin xs\n"
         ++ "  where y = length xs\n") 
            "wokatz"
            fromBinRecurS
            ""

fromBin96 = 
  Solution ("fromBin []      = 0\n"
         ++ "fromBin (x:y)   = x * 2 ^ length y + fromBin y\n") 
            "ybouma"
            fromBinRecurS
            ""
