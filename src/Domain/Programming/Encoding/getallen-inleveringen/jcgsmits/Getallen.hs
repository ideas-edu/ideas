--Gemaakt door Jeroen Smits
--Studentnr: 3344932
--Met behulp van de Helium compiler
fromDec       :: [Int] -> Int
numLength     :: Int -> Int -> Int -> Int
toDec         :: Int -> [Int]
tDec          :: Int -> Int -> [Int]
fromBin       :: [Int] -> Int
toBin         :: Int -> [Int]
tBin          :: Int -> Int -> [Int]
fromBase      :: Int -> [Char] -> Int
charToInt     :: Char -> Int
toBase        :: Int -> Int -> [Char]
tBase         :: Int -> Int -> Int -> [Char]
intToChar     :: Int -> Char
numbers       :: Int -> [String] -> [(String, Int)]
charControl   :: Int -> [Char] -> Bool
lookAndSay    :: Int -> [String]
lookAndSay'   :: [Int] -> [Int] -> [Int] -> [Int]
keithGetallen :: [Int]
keithGetal    :: Int -> [Int]
keith         :: Int -> Bool
keith'        :: Int -> [Int] -> Int -> Bool
keith''       :: [Int] -> Int -> Int

fromDec []     = 0
fromDec (x:xs) = let y = length xs
                 in if(x>=0 && x<=9) then x * 10 ^ y + fromDec xs else error "niet decimaal"

numLength x y z = if (x `mod` y == x) then y `div` z
                  else numLength x (y * z) z

toDec x = let y = numLength x 10 10
              z = x `div` y
          in if (y == 1) then [z] else z : tDec (x - z * y) (y `div` 10)

tDec x y = let z = x `div` y
           in if (y == 1) then [z] else z : tDec (x - z * y) (y `div` 10)

fromBin [] = 0
fromBin (x:xs) = let y = length xs
                 in if (x == 1 || x == 0) then x * 2 ^ y + fromBin xs else error "niet binair"

toBin x = let y = numLength x 2 2
              z = x `div` y
          in if (y == 1) then [z] else z : tBin (x - z * y) (y `div` 2)

tBin x y = let z = x `div` y
           in if (y == 1) then [z] else z : tBin (x - z * y) (y `div` 2)

fromBase _ [] = 0
fromBase g (x:xs) = let y = length xs
                    in if(charToInt x < g) then charToInt x * g ^ y + fromBase g xs else error "digit to great"

charToInt c | c >= '0' && c <= '9' =  fromEnum c - fromEnum '0'
            | c >= 'a' && c <= 'z' =  fromEnum c - fromEnum 'a' + 10
            | c >= 'A' && c <= 'Z' =  fromEnum c - fromEnum 'A' + 10
            | otherwise            =  error "not a digit"

toBase g x = let y  = numLength x g g
                 z  = intToChar (x `div` y)
                 zz = x `div` y
             in if (y == 1) then [z] else z : tBase g (x - zz * y) (y `div` g)

tBase g x y = let z  = intToChar (x `div` y)
                  zz = x `div` y
              in if (y == 1) then [z] else z : tBase g (x - zz * y) (y `div` g)

intToChar x | x >= 0 && x < 10    = toEnum (fromEnum '0' + x)
            | otherwise = toEnum (fromEnum 'A' - 10 + x)

numbers _ []     = []
numbers g (x:xs) = if (charControl g x) then (x, fromBase g x) : numbers g xs else numbers g xs

charControl _ []     = False
charControl g [x]    = if (charToInt x < g) then True else False
charControl g (x:xs) = if (charToInt x < g) then charControl g xs else False
                      
lookAndSay x = let y = fromDec (reverse (lookAndSay' (reverse (toDec x)) [] []))
                   z = [map intToChar(toDec x)]
               in z ++ lookAndSay y

lookAndSay' (x:xs) [] [] = lookAndSay' xs [x, 1] []
lookAndSay' (x:xs) [y, ys] [] = if (x == y) then lookAndSay' xs [y, ys + 1] []
                                else lookAndSay' xs [x, 1] [y, ys]
lookAndSay' (x:xs) [y, ys] z = if (x == y) then lookAndSay' xs [y, ys + 1] z
                               else lookAndSay' xs [x, 1] z ++ [y, ys]
lookAndSay' [] [y, ys] z = z ++ [y, ys]
lookAndSay' _ _ _ = error "lookAndSay' niet bruikbaar met deze waarde"

keithGetallen = keithGetal 10

keithGetal x = if (keith x) then x : keithGetal (x + 1) else keithGetal (x + 1)

keith x = let y = toDec x
              z = length y
          in keith' x y z
keith' x y z = let yy = keith'' y z
             in if (yy == x) then True else (if (yy > x) then False else keith' x (y ++ [yy]) z)

keith'' x y = if (y == 1) then last x else last x + keith'' (init x) (y - 1)
              