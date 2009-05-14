module Domain.Programming.EncodingExercises where

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

