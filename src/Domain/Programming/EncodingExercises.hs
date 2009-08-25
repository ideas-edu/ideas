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

fromBins = [fromBin,fromBin1,fromBin2,fromBin3,fromBin5,fromBin6,fromBin7,fromBin8,fromBin9,fromBin10,fromBin11,fromBin12,fromBin13,fromBin14,fromBin15,fromBin16,fromBin17,fromBin18,fromBin19,fromBin20,fromBin21,fromBin22,fromBin23,fromBin25,fromBin26,fromBin27,fromBin28,fromBin29,fromBin30,fromBin31,fromBin32,fromBin33,fromBin34,fromBin35,fromBin36,fromBin37,fromBin38,fromBin40,fromBin41,fromBin42,fromBin43,fromBin44,fromBin45,fromBin46,fromBin47,fromBin48,fromBin49,fromBin50,fromBin51,fromBin52,fromBin53,fromBin54,fromBin55,fromBin56,fromBin57,fromBin58,fromBin59,fromBin60,fromBin61,fromBin62,fromBin63,fromBin64,fromBin65,fromBin66,fromBin67,fromBin68,fromBin69,fromBin70,fromBin71,fromBin72,fromBin73,fromBin74,fromBin75,fromBin76,fromBin77,fromBin78,fromBin79,fromBin80,fromBin81,fromBin82,fromBin83,fromBin84,fromBin85,fromBin86,fromBin87,fromBin88,fromBin89,fromBin90,fromBin91,fromBin92,fromBin93,fromBin94,fromBin95,fromBin96]

fromBinsNoCompile = [fromBin24 {- can be fixed by inlining mapAccumR -}]
fromBinsWrong = [fromBin4]


-- Stefan his standard solution
fromBin = ("fromBin = foldl ((+) . (* 2)) 0", "Stefan")


-- Test solutions
fromBinER = ("fromBin = f 0\n" -- explicit recursion
          ++ "  where\n" 
          ++ "    f nil []     = nil\n" 
          ++ "    f nil (x:xs) = f (((+) . (*2)) nil x) xs\n", "Explicit recursion")

fromBinLet = ("fromBin = let f nil []     = nil\n" 
           ++ "              f nil (x:xs) = f (((+) . (*2)) nil x) xs\n"
           ++ "          in f 0\n", "Let variant")

fromBinEta = ("fromBin = foldl (\\x -> (\\y -> ((+) . (* 2)) y) x) 0", "Eta expanded variant")


-- Student solutions:

fromBin1 = ("fromBin = fromBase' 2\n"
         ++ "fromBase' _ []     = 0\n"
         ++ "fromBase' n (x:xs) = x * n ^ length xs + fromBase' n xs\n", "affboth-lveerman")

fromBin2 = ("fromBin = foldl (\\x y -> x * 2 + y) 0", "bdoren-mjspoor")

fromBin3 = ("fromBin [] = 0\n"
         ++ "fromBin (x:xs) = x * 2^(length xs) + fromBin xs\n", "bgreeven-jsteenbe")

-- looks like a foldl1, however, solution is not entirely correct (no case for [])
fromBin4 = ("fromBin [x]      = x\n"
         ++ "fromBin (x:y:ys) = fromBin (x * 2 + y : ys)\n", "bjliefer-ptpkokke")

fromBin5 = ("fromBin = fromBaseInt 2\n"
         ++ "fromBaseInt base xs = sum $ zipWith (*) bMachten xs\n"
         ++ "  where bMachten = scanr (*) 1 $ take (length xs - 1) $ repeat base\n", "bspaans")

fromBin6 = ("fromBin [] = 0\n"
         ++ "fromBin (x:xs) = (x*(2^length xs)) + fromBin xs\n", "btdijk")

fromBin7 = ("fromBin = foldl ( (+).(2*) ) 0", "cjblom")

fromBin8 = ("fromBin [] = 0\n"
         ++ "fromBin (h:hs) =h*2^length hs +fromBin hs\n", "ddtoniss-dsgroote")

fromBin9 = ("fromBin l = foldr op 0 (reverse l)\n"
         ++ "  where op a b = a + 2*b\n", "dekuppev")

fromBin10 = ("fromBin [] = 0\n"
          ++ "fromBin x@(y:ys) = y * 2^(length x - 1) + fromBin ys\n", "dgerritz")

fromBin11 = ("fromBin []       =  0\n"
          ++ "fromBin (x : xs) =  x * 2 ^ (length xs) + fromBin xs\n", "dpboot")

fromBin12 = ("fromBin [] = 0\n"
          ++ "fromBin (x:xs) = x * (2^(length xs)) + fromBin xs\n", "dwinkel")

fromBin13 = ("fromIntBase b = foldl' (\\ x y -> b*x+y ) 0\n"
          ++ "fromBin = fromIntBase 2\n", "elrenkem-ogrottie")

fromBin14 = ("fromBin   = foldl (\\r n -> 2 * r + n) 0\n", "ergallo")

fromBin15 = ("fromNumeralSystem base input\n"
          ++ "    | and (map (<base) input) = foldl ((+).(base*)) 0 input\n"
          ++ "    | otherwise = error \"Input not within limits of base\"\n"
          ++ "fromBin = fromNumeralSystem 2\n", "ewjmulde")

fromBin16 = ("fromBin [] = 0\n"
          ++ "fromBin (x:xs) |x == 1 = (2^(length xs)) + fromBin xs\n"
          ++ "               |otherwise = fromBin xs\n", "fcbijlsm")

fromBin17 = ("fromBin = digitsToInt 2\n"
          ++ "digitsToInt b dgts = if (null.(filter (\\x -> 0>x || x>=b))) dgts && (not.null) dgts\n"
          ++ "                     then foldl (\\x y -> b*x + y) 0 dgts\n"
          ++ "                     else error (\"digitsToInt: list must not be empty and list must only include values: 0 - \" ++ show(b-1))", "fldenis")

fromBin18 = ("fromBin [] = 0\n"
          ++ "fromBin [s] = s\n"
          ++ "fromBin (s:t:staart) = fromBin ((2*s+t):staart)\n", "fsteeg-hkbarnev")

fromBin19 = ("fromBin  []    = 0\n"
          ++ "fromBin (x:xs) = 2 ^ (length (x:xs) -1 ) * x + fromBin xs", "gcpzunde")

fromBin20 = ("fromBin = fromBaseI 2\n"
          ++ "fromBaseI base = foldl op 0\n"
          ++ "    where op a b | abs b < base = (abs a * base + abs b) * sign a * sign b\n"
          ++ "                 | otherwise = error \"Invoer cijfers groter dan basis.\"\n"
          ++ "              where sign x | x < 0 = -1\n"
          ++ "                           | otherwise = 1\n", "gdijkstr-rjhensin")

fromBin21 = ("fromBin []     = 0\n"
          ++ "fromBin (x:xs) = x*2^length(xs) + fromBin xs\n", "gloupias")

fromBin22 = ("fromBin [] = 0\n"
          ++ "fromBin (x:xs) = x * 2 ^ length xs + fromBin xs\n", "hjkuijk")

fromBin23 = ("fromBin = fromBaseIndex 2\n"
          ++ "fromBaseIndex _ [] = 0\n"
          ++ "fromBaseIndex base (i:is) = base^(length is)*i + fromBaseIndex base is\n", "hlversto")

fromBin24 = ("fromBin = fromSys 2\n"
          ++ "fromSys s l = foldl (+) 0 (snd (mapAccumR convertNthDigit 0 l))\n"
          ++ "            where convertNthDigit x y | y < s     = (x+1, y*s^x)\n"
          ++ "                                      | otherwise = error \"number is greater then base\"\n", "hmpaasse")

fromBin25 = ("fromBin []     = 0\n"
          ++ "fromBin (x:xs) = x * 2^l + fromBin xs\n"
          ++ "  where l = length xs\n", "imberg-cwbbonen")

fromBin26 = ("fromBin s = fromBinMetHulp s 0\n"
          ++ "fromBinMetHulp [] hulp = hulp\n"
          ++ "fromBinMetHulp (x:xs) hulp = fromBinMetHulp xs ((hulp*2) + (x))\n", "jcgoosen")

fromBin27 = ("fromBin [] = 0\n"
          ++ "fromBin (x:xs) = let y = length xs\n"
          ++ "                 in if (x == 1 || x == 0) then x * 2 ^ y + fromBin xs else error \"niet binair\"\n", "jcgsmits")

fromBin28 = ("fromBin = foldl (\\x y -> 2*x+y) 0", "jdfeddem")

fromBin29 = ("fromBin [] =  0\n"
          ++ "fromBin (x:xs) =  x * 2 ^ length xs + fromBin xs\n", "jduijn-iduijn")

fromBin30 = ("fromBin [] = 0\n"
          ++ "fromBin (x:xs) = x * 2 ^ (length xs) + fromBin xs\n", "jeceding")

fromBin31 = ("fromBin ints = fromBinHulp 0 (reverse ints)\n"
          ++ "  where fromBinHulp _ [] = 0\n"
          ++ "        fromBinHulp n (i:is) | i < 2     = (i*2^n) + fromBinHulp (n+1) is\n"
          ++ "                             | otherwise = undefined\n", "jgageldo")

fromBin32 = ("fromBin [] = 0\n"
          ++ "fromBin (x:xs) = 2^(length xs) * x + (fromBin xs)\n", "jhberg")

fromBin33 = ("fromBin = fromBaseN 2\n"
          ++ "fromBaseN base number = fromBaseN' base (reverse number)\n"
          ++ "    where fromBaseN' _ [] = 0\n"
          ++ "          fromBaseN' base' (c:cs) = c + base' * (fromBaseN' base' cs)\n", "jhorn")

fromBin34 = ("fromBin [] = 0\n"
          ++ "fromBin (a:b) = let macht = 2^(length (b))\n"
          ++ "                in a * (macht) + fromBin b\n", "jjhoozem")

fromBin35 = ("fromBin [] = 0\n"
          ++ "fromBin (x:xs) = x * 2 ^ n + fromBin xs\n"
          ++ "  where n = length xs\n", "jkoperdr")

fromBin36 = ("fromBin []     = 0\n"
          ++ "fromBin (x:xs) = x*b + fromBin xs\n"
          ++ "  where b = 2^length xs\n", "jleersum-anieuwla")

fromBin37 = ("fromBin x = fromBin' x (length x - 1)\n"
          ++ "  where fromBin' []     _ = 0\n"
          ++ "        fromBin' (y:ys) z | y == 0 || y == 1 = y * 2^z + fromBin' ys (z-1)\n"
          ++ "                          | otherwise        = error \"Alstublieft binaire getallen gebruiken\"\n", "jmlinsse")

fromBin38 = ("fromBin []     =  0\n"
          ++ "fromBin (x:xs) =  x * 2^(length xs) + fromBin xs\n", "jmulder")

fromBin40 = ("fromBin [] = 0\n"
          ++ "fromBin (x : xs) = x * 2 ^ (length xs) + fromBin xs\n", "jtkman-echgbon")

fromBin41 = ("fromBin [] = 0\n"
          ++ "fromBin (x : xs) = x*2^length xs + fromBin xs\n", "jwind")

fromBin42 = ("fromBin [] = 0\n"
          ++ "fromBin (x:xs) = result + fromBin xs\n"
          ++ "  where result = x * (2 ^ length xs)\n", "kfaro")

fromBin43 = ("fromBin [] = 0\n"
          ++ "fromBin (x : []) = x\n"
          ++ "fromBin (x : xs) = ( x * 2^length xs ) + fromBin xs\n", "kjdvoors-apol")

fromBin44 = ("fromBin (a : as) = a*(2^(length(a : as)-1)) + fromBin(as)\n"
          ++ "fromBin [] = 0\n", "lblhartm")

fromBin45 = ("fromBin x = fromBin' x (length x - 1)\n"
          ++ "fromBin' [] _ = 0\n"
          ++ "fromBin' (x:xs) y = x*2^y + (fromBin' xs (y - 1))\n", "ldsbroe-jfklein")

fromBin46 = ("fromBin []     = 0\n"
          ++ "fromBin (x:xs) = x * 2^length(xs) + fromBin xs\n", "lrwester")

fromBin47 = ("fromBin (x:xs) = x * 2^(length xs) + fromBin xs\n"
          ++ "fromBin []     = 0\n", "lsstoel")

fromBin48 = ("fromBin [] = 0\n"
          ++ "fromBin xs = fromBin' 0 (reverse xs)\n"
          ++ "fromBin' _ [] = 0\n"
          ++ "fromBin' a (x:xs) = 2^a * (abs x `mod` 2) + fromBin' (a+1) xs\n", "ltbinsbe")

fromBin49 = ("fromBin = foldl ((+).(*2)) 0", "lwgraaff")

fromBin50 = ("fromBin []   = 0\n"
          ++ "fromBin l    = sum (oplopendeMachten 2 l)\n"
          ++ "oplopendeMachten a []=  [0]\n"
          ++ "oplopendeMachten a l =  last l: oplopendeMachten a (init(basis))\n"
          ++ "  where basis = map (*a) l\n", "mahashi-mjhobbel")

fromBin51 = ("fromBin []     = 0\n"
          ++ "fromBin (x:xs) | x==0 || x==1 = x * 2 ^ (length xs) + (fromBin xs)\n"
          ++ "               | otherwise    = error \"Binaire getallen hebben alleen nullen en enen, dommerd!\"\n", "maooster")

fromBin52 = ("fromBin [] = 0\n"
          ++ "fromBin [bin] = bin\n"
          ++ "fromBin (bin:rest) = bin * 2^length rest + fromBin rest\n", "mbarendr")

fromBin53 = ("fromBin = foldl (\\n c -> 2 * n + c) 0", "mfrancke-jfidijks")

fromBin54 = ("fromBin x = fromBinRev (reverse x) 0\n"
          ++ "fromBinRev [] _ = 0\n"
          ++ "fromBinRev [x] y  = x * 2^y\n"
          ++ "fromBinRev (x:xs) y = x * 2^y + fromBinRev xs (y+1)\n", "mgrimme")

fromBin55 = ("fromBin = fromSpecial 2\n"
          ++ "fromSpecial _ [] = 0\n"
          ++ "fromSpecial base xs | or (map (\\y -> y >= base || y<0) xs) = error \"Please provide a list only containing values in the given range\"\n"
          ++ "                    | otherwise = sum (zipWith (*) (reverse xs) [base^x | x <- [0..] ])\n", "mkroese")

fromBin56 = ("fromBin [] = 0\n"
          ++ "fromBin getalLijst = 2 * fromBin (init getalLijst) + last getalLijst\n", "mlmbroer-jmwbrete")

fromBin57 = ("fromBin = foldl (\\x y -> 2*x + y) 0\n", "mmontvai-beck")

fromBin58 = ("fromBin a = from 2 a\n"
          ++ "from a b | (or . (map (a<))) b = error \"[Int]->Int conversion out of range\"\n"
          ++ "         | otherwise         = from'' a b\n"
          ++ "from'' a [] = 0\n"
          ++ "from'' a [b] = b\n"
          ++ "from'' a ( kop : tussen : staart ) = from'' a ([(kop * a) + tussen ] ++ staart)\n", "mrvaarti-rwerken")

fromBin59 = ("fromBin []      = 0\n"
          ++ "fromBin (x:xs)  = x * 2 ^ length xs + fromBin xs\n", "mtduysen")

fromBin60 = ("fromBin [] = 0\n"
          ++ "fromBin xs = fromBim (reverse xs)\n"
          ++ "fromBim [] = 0\n"
          ++ "fromBim (x:xs) | x < 0 || x > 1 = error \"Niet binair getal!\"\n"
          ++ "               | otherwise = x + 2 * fromBim xs\n", "mtibboel-sabitter")

fromBin61 = ("fromBin x = maakGetal 2 x 0\n"
          ++ "maakGetal _ [] b = b\n"
          ++ "maakGetal a (x:xs) b = if (x < a) then  maakGetal a xs (b * a + x) else 0\n", "mvensela-merboxel")

fromBin62 = ("fromBin [] = 0\n"
          ++ "fromBin (x : xs) = x * 2 ^ length xs + fromBin xs\n", "mzwan-jgeeden")

fromBin63 = ("fromBin [0] = 0\n"
          ++ "fromBin [1] = 1\n"
          ++ "fromBin(h:t) = (h*(2^length t)) + fromBin t\n", "njvlamin")

fromBin64 = ("fromBin a = foldl (\\x y->(2*x)+y) 0 a\n", "nroumimp")

fromBin65 = ("fromBin = fromAap 2\n"
          ++ "fromAap base = foldl (nextInt base) 0\n"
          ++ "  where nextInt base x y | y >= base = error (concat[\"fromAap: Number \", show y, \" out of range for base \", show base,\".\"])\n"
          ++ "                         | y < 0 = error (concat[\"fromAap: Number \", show y, \" is negative.\"])\n"
          ++ "                         | otherwise = x * base + y\n", "ompennin")

fromBin66 = ("fromBin [] = 0\n"
          ++ "fromBin (x:xs) = fromBin' xs x\n"
          ++ "fromBin' [] tussen = tussen\n"
          ++ "fromBin' (x:xs) tussen = fromBin' xs (2*tussen+x)\n", "pdstaats-cjplatte")


fromBin67 = ("fromBin    x     =  toInt 2 x\n"
          ++ "toInt    _      []                           = 0\n"
          ++ "toInt    b      (x:xs)  | x < 0 || x > b - 1 = -1\n"
          ++ "                        | otherwise          = x * (b ^ l) + toInt b xs\n"
          ++ "                          where l = length xs\n", "pjwjanse-jjvisser")

fromBin68 = ("fromBin xs = foldl' (\\x y -> x*2+y) 0 xs\n", "pmspek")

fromBin69 = ("fromBin [] = 0\n"
          ++ "fromBin (x:xs) = x*(2^length xs)+fromBin xs\n", "pqgroot")

fromBin70 = ("fromBin cs = if isBinair cs then foldl (\\ x y -> x*2 + y) 0 cs else error \"Invoer is niet Binair!\"\n"
          ++ "isBinair [] = True\n"
          ++ "isBinair (c:cs) | c == 1 || c == 0 = isBinair cs\n"
          ++ "                | otherwise = False\n", "raspauwe")

fromBin71 = ("fromBin [] = 0\n"
          ++ "fromBin (x:xs)  | x <= 1 = x * (2 ^ (length (x:xs) - 1)) + fromBin xs\n"
          ++ "                | otherwise = error \"lijst mag alleen uit 1 en 0 bestaan!\"\n", "ravries-ccmrooij")

fromBin72 = ("fromBin []     = 0\n"
          ++ "fromBin (x:xs) = x * 2^length xs + fromBin xs\n", "rawagenm")

fromBin73 = ("fromBin [] = 0\n"
          ++ "fromBin (x:xs) = (x*b)+ fromBin(xs)\n"
          ++ "   where b = 2^(length(xs))\n", "rehoef-irrencke")

fromBin74 = ("fromBin x = fromIntGet 2 x\n"
          ++ "fromIntGet _ [] = 0\n"
          ++ "fromIntGet g (x:xs) =  g^(length xs) * x + fromIntGet g xs\n", "rgroot")

fromBin75 = ("fromBin = fromBaseInt 2\n"
          ++ "fromBaseInt n = (foldr (\\x y -> x + n*y) 0) . reverse\n", "rhaan")

fromBin76 = ("fromBin [] = 0\n"
          ++ "fromBin [x] = x\n"
          ++ "fromBin (x:xs)= getal + fromBin xs\n"
          ++ "  where getal = x * (2 ^ length xs)\n", "rhouweli")

fromBin77 = ("fromBin [] = 0\n"
          ++ "fromBin (c:cs) = c * (2 ^ length cs) + fromBin cs\n", "rpvermeu-jcrvrijho")

fromBin78 = ("fromBin [] = 0\n"
          ++ "fromBin (x:xs) =   x * 2^length xs + fromBin xs\n", "rslagmol")

fromBin79 = ("fromBin [] = 0\n"
          ++ "fromBin (x:xs) = x*2^(length xs) + fromBin xs\n", "rtharder")

fromBin80 = ("fromBin [] = 0\n"
          ++ "fromBin (x:xs) = x * 2 ^ length xs + fromBin xs\n", "rvesten-rsalphen")

fromBin81 = ("fromBin [] = 0\n"
          ++ "fromBin (x:xs) = x*2^(length xs) + fromBin xs\n", "sagieske")

fromBin82 = ("fromBin [] = 0\n"
          ++ "fromBin (x:xs) = (x * 2 ^ length (xs))+ fromBin (xs)\n", "sdriel")

fromBin83 = ("fromBin [] = 0\n"
          ++ "fromBin (x:xs) = x * 2 ^ length xs  + fromBin xs\n", "sjavissc")

fromBin84 = ("fromBin [] = 0\n"
          ++ "fromBin (x:xs) = x * 2 ^ z + fromBin xs\n"
          ++ "  where z = length(x:xs) - 1\n", "sjveldhu")

fromBin85 = ("fromBin l = sum $ zipWith (*) (map (2^) [0..(length l)-1]) (reverse l)\n"
            , "sttimmer")

fromBin86 = ("fromBin [a] = a\n"
          ++ "fromBin (a:as) = a * (2 ^ (length (a:as) - 1)) + fromBin(as)\n", "talles")

fromBin87 = ("fromBin x = doFromBin (reverse x) 0\n"
          ++ "  where doFromBin (y:ys) z = y * 2^z + doFromBin ys (z +1)\n"
          ++ "        doFromBin [] _ = 0\n", "taveld")

fromBin88 = ("fromBin x = fromBin2 x ((length x) - 1)\n"
          ++ "fromBin2 (a:[]) _ = a\n"
          ++ "fromBin2 (h:t) x = (h * (2 ^ x)) + fromBin2 t (x - 1)\n", "tleroi")

fromBin89 = ("fromBin = listToNumber 2\n"
          ++ "listToNumber _ [] = 0\n"
          ++ "listToNumber stelsel (h:t) = h*(stelsel^(length t)) + (listToNumber stelsel t)\n"
            , "tmsoetho")

fromBin90 = ("fromBin ys = fB ys (length ys - 1)\n"
          ++ "  where fB [] _ = 0\n"
          ++ "        fB (x:xs) n = (x * 2^n) + fB xs (n-1)\n", "tromberg-rjanssen")

fromBin91 = ("fromBin' (h:[]) _   = h\n"
          ++ "fromBin' (h:t) l = (h * (2 ^ l)) + (fromBin' t (l - 1))\n"
          ++ "fromBin v = fromBin' v ((length v) - 1)\n", "tsteemer")

fromBin92 = ("fromBin [] = 0\n"
          ++ "fromBin (x:xs) = x * (2 ^ l) + fromBin xs\n"
          ++ "  where l = length (x:xs) -1\n", "vrbons-hckampma")

fromBin93 = ("fromBin [] = 0\n"
          ++ "fromBin (x:xs) = x * (2 ^ (length xs)) + fromBin xs\n", "whustinx")

fromBin94 = ("fromBin list = fromBaseNum 2 list\n"
          ++ "fromBaseNum b list = sum (zipWith (*) (reverse list) (powList b))\n"
          ++ "  where powList b = pow 0 where pow n = (b^n):(pow (n+1))\n", "wlelsing")

fromBin95 = ("fromBin [] = 0\n"
          ++ "fromBin (x:xs) = x*2^y + fromBin xs\n"
          ++ "  where y = length xs\n", "wokatz")

fromBin96 = ("fromBin []      = 0\n"
          ++ "fromBin (x:y)   = x * 2 ^ length y + fromBin y\n", "ybouma")


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

