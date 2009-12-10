-- Sjoerd T. Timmer
-- 3118479 sttimmer
-- ghc 6.10.1
import Char -- i'll be using ord
import List 

-- this is what i'm going to do:
fromDec  :: [Int] -> Int
toDec    :: Int -> [Int]
fromBin  :: [Int] -> Int
toBin    :: Int -> [Int]
fromBase :: Int -> [Char] -> Int
toBase :: Int -> Int -> [Char]
numbers :: Int -> [String] -> [(String,Int)]
grayCode :: Int -> ([Char]->Int,Int->[Char])
lookAndSay :: Int -> [String]
keithGetallen :: [Int]

-- fromDec :: [Int] -> Int
fromDec [] = 0 -- every digit count for 10^(it's position)
fromDec (x:xs) = x * 10 ^ (length xs) + fromDec xs

-- toDec :: Int -> [Int]
toDec 0 = [0]  -- we don't want [] in this case
toDec d = reverse (toDec' d) where
    toDec' 0 = []
    toDec' x =  (x `mod` 10) : toDec' (x `div` 10)
    -- (x mod 10) is the last digit, 
    -- (x div 10) is the rest of the digits shifted right

-- fromBin :: [Int] -> Int
fromBin l = sum $ zipWith (*) (map (2^) [0..(length l)-1]) (reverse l)
-- we create a list of powres of 2 and zip it through (*) with l

-- toBin :: Int -> [Int]
toBin b = reverse (toBin' b) where
          toBin' 0 = [0]
          toBin' 1 = [1]
          toBin' bin = (bin `mod` 2) : toBin' (bin `div` 2)
          -- similar to toDec, only with powers of 2
          -- and another starting definition

-- help-function for fromBase:
-- charToInt maps '3' to 3 and 'b' to 11, but also 'h' to 17
charToInt :: Char -> Int
charToInt d | ord d < 58 = ord d - 48
            | otherwise  = ord d - 87

-- fromBase :: Int -> [Char] -> Int
fromBase b xs = sum $ zipWith (*) 
                              (map (b^) [0..(length xs)-1]) 
                              (map charToInt (reverse xs))
-- the same as fromBin, only with a variable base

-- help-function for toBase:
-- intToChar is the reverse of charToInt declared just above
intToChar :: Int -> Char
intToChar i = "0123456789abcdefghijklmnopqrstuvwxyz"!!i
-- i think this is pretty ugly, but i really wouldn't know of a better
-- way to do this...

-- toBase :: Int -> Int -> [Char]
toBase _ 0 = ['0'];
toBase b x = reverse (toBase' b x) where
    toBase' b 0 = []
    toBase' b x = (intToChar (x `mod` b)) : (toBase' b (x `div` b))

-- numbers :: Int -> [String] -> [(String,Int)]
numbers b xs = map (\a -> (a,fromBase b a)) 
                   (filter (\x -> (maximum (map charToInt x)<b)) xs)
               -- first filter out Strings that contain 'digits' higher
               -- or equal to the base we want to use. Then apply a map
               -- that generates the desired tuples

-- gray coding not yet finished:s
-- encoding works fine, but i really don't know how to decode again
toGray :: Int -> Int -> [Char]
toGray _ 0 = ['0'] -- in real world this is reasonable
toGray b i = (reverse (toGray' b i)) where
    toGray' :: Int -> Int -> [Char]
    toGray' _ 0 = [] -- in the itteration this is the empty list
    toGray' b i = intToChar (
                  ([0..b-1]++reverse[0..b-1]) !! -- first up then down
                                  (i `mod` (b*2))  ) 
                  : toGray' b (i `div` b)

-- well it has to return something...
fromGray :: Int -> [Char] -> Int
fromGray _ _ = 42

-- grayCode :: Int -> ([Char]->Int,Int->[Char])
grayCode b = (fromGray b,toGray b)

-- lookAndSay :: Int -> [String]
lookAndSay i = look (map intToDigit (toDec i)) where
    say :: String -> String -- how to generate the next
    look :: String -> [String] -- the real loop
    say s = map intToDigit 
                (foldl (++) [] 
                (map (\a -> [length a,a!!0]) 
                     (group (map digitToInt s)) ))
    -- we convert to ints, then group, and map a function that generates
    -- a list of 2 elements containing the length and one of the
    -- elements. foldl (++) [] flattens the result which then is
    -- converted back to digits
    look s = s:look (say s) -- and keep going forever
    

-- help-functions for keithGetallen
isKeith :: Int -> Bool
keithGen :: Int -> [Int]

-- keithGen 
keithGen i = map (!!0) (generator (toDec i)) where
    generator = iterate (\l -> (tail l)++[sum l]) -- expensive, i know
    -- like fibonacci with tuples, only now with variable length lists
    -- the length of these lists(determinded by the length of the first)
    -- determines how many numbers will be summed to create the next

-- isKeith: this should speak for itself:
isKeith i = takeNext (keithGen i) where
    takeNext (x:xs) | x >  i = False -- nope, sory you lose!
                    | x == i = True  -- yes, we got one!
                    | otherwise = takeNext xs -- go on, we're not there yet...

-- keithGetallen: and now this is very easy
keithGetallen = filter isKeith [10..]


-- to demonstrate that it is all working:

main = do
    print "testing toDec with [0..10]++[98..102]"
    printList (map toDec ([0..10]++[98..102]))

    print "testing fromDec with the result of toDec"
    printList (map fromDec (map toDec ([0..10]++[98..102])))

    print "testing toBin with [0..10]:"
    printList (map toBin [0..10])

    print "testing fromBin with the result of toBin"
    printList (map fromBin (map toBin [0..10]))

    print "testing fromBase with 423@10,100110@2,f4c8@16 and 5j@23:"
    print (fromBase 10 "423")
    print (fromBase 2 "100110")
    print (fromBase 16 "f4c8")
    print (fromBase 23 "5j")

    print "testing toBase with :423@10, 38@2, 62664@16 and 134@23"
    print (toBase 10 423)
    print (toBase 2 38)
    print (toBase 16 62664)
    print (toBase 23 134)

    print "testing numbers:"
    printList (numbers 16 ["ace","face","faces"])

    print "testing lookAndSay:"
    printList (take 10 (lookAndSay 1))

    print "testing the first 25 keithNumbers"
    printList (take 25 keithGetallen)
    
    print "testing toGray in base 4"
    print (map (snd (grayCode 4)) [0..30])

-- this function is nice for debugging:
-- Prelude probably already has something like it
-- but I just felt like rewriting it for practice
printList :: Show a => [a] -> IO()
printList [] = return ()
printList (x:xs) = do
    print x
    printList xs



