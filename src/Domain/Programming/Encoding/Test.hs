import qualified Getallen as G
import Data.List
import Test.QuickCheck

-------------------------------------------------------------------------------
-- Models
-------------------------------------------------------------------------------

class (Ord a) => Digit a where zero :: a
instance Digit Int where zero = 0
instance Digit Char where zero = '0'

pad :: Digit a => [a] -> [a] -> ([a], [a])
pad ds ds'
  | n < 0     = (replicate n zero ++ ds, ds')
  | n > 0     = (ds, replicate n zero ++ ds')
  | otherwise = (ds, ds')
  where
    n = length ds - length ds'

trim :: Digit a => [a] -> [a]
trim (d : ds@(_ : _)) | d == zero = trim ds
trim ds                           = ds

fromDigit :: Char -> Int
fromDigit '0' = 0  ; fromDigit 'c' = 12 ; fromDigit 'o' = 24
fromDigit '1' = 1  ; fromDigit 'd' = 13 ; fromDigit 'p' = 25
fromDigit '2' = 2  ; fromDigit 'e' = 14 ; fromDigit 'q' = 26
fromDigit '3' = 3  ; fromDigit 'f' = 15 ; fromDigit 'r' = 27
fromDigit '4' = 4  ; fromDigit 'g' = 16 ; fromDigit 's' = 28
fromDigit '5' = 5  ; fromDigit 'h' = 17 ; fromDigit 't' = 29
fromDigit '6' = 6  ; fromDigit 'i' = 18 ; fromDigit 'u' = 30
fromDigit '7' = 7  ; fromDigit 'j' = 19 ; fromDigit 'v' = 31
fromDigit '8' = 8  ; fromDigit 'k' = 20 ; fromDigit 'w' = 32
fromDigit '9' = 9  ; fromDigit 'l' = 21 ; fromDigit 'x' = 33
fromDigit 'a' = 10 ; fromDigit 'm' = 22 ; fromDigit 'y' = 34
fromDigit 'b' = 11 ; fromDigit 'n' = 23 ; fromDigit 'z' = 35

fromBin, fromDec :: [Int] -> Int
fromBin ds = sum (zipWith (\d e -> d *  2 ^ e) (reverse ds) [0 ..])
fromDec ds = sum (zipWith (\d e -> d * 10 ^ e) (reverse ds) [0 ..])

fromBase :: Int -> [Char] -> Int
fromBase b ds = sum (zipWith (\d e -> fromDigit d * b ^ e) (reverse ds) [0 ..])

look :: String -> [(Int, Char)]
look = map (\cs@(c : _) -> (length cs, c)) . group

say :: [(Int, Char)] -> String
say = concatMap $ \ (n, c) -> show n ++ [c]

keith :: Int -> [Int]
keith n = let ns = map (read . (: [])) (show n) in ns ++ keith' ns
  where
    keith' ns = let n = sum ns in n : keith' (tail ns ++ [n])

isKeithNumber :: Int -> Bool
isKeithNumber n = head (dropWhile (< n) (keith n)) == n

keithNumbers :: [Int]
keithNumbers = filter isKeithNumber [10 ..]

-------------------------------------------------------------------------------
-- Generators
-------------------------------------------------------------------------------

newtype BinDigits = BinDigits [Int]      deriving Show
newtype DecDigits = DecDigits [Int]      deriving Show
newtype Base      = Base      Int        deriving Show
data    Digits    = Digits    Int [Char] deriving Show
newtype Words     = Words     [String]   deriving Show

instance Arbitrary BinDigits where
  arbitrary = do
    ds <- listOf1 . oneof . map return $ [0, 1]
    return (BinDigits $ trim ds) 

instance Arbitrary DecDigits where
  arbitrary = do
    ds <- listOf1 . oneof . map return $ [0 .. 9]
    return (DecDigits $ trim ds)

instance Arbitrary Base where
  arbitrary = fmap Base (oneof . map return $ [2 .. 36])

instance Arbitrary Digits where
  arbitrary = do
    Base b <- arbitrary
    ds <- listOf1 . oneof . map return . take b $ ['0' .. '9'] ++ ['a' .. 'z']
    return (Digits b $ trim ds)

instance Arbitrary Words where
  arbitrary = fmap Words (listOf . listOf1 . oneof . map return $ ['a' .. 'z'])

-------------------------------------------------------------------------------
-- Properties
-------------------------------------------------------------------------------

propFromBin  (BinDigits ds)          = G.fromBin ds == fromBin ds
propToBin    (NonNegative n)         = fromBin (G.toBin n) == n
propFromDec  (DecDigits ds)          = G.fromDec ds == fromDec ds
propToDec    (NonNegative n)         = fromDec (G.toDec n) == n
propFromBase (Digits b ds)           = G.fromBase b ds == fromBase b ds
propToBase   (Base b, NonNegative n) = fromBase b (G.toBase b n) == n 

propNumbersSound (Base b, Words ws) =
  and [fromBase b [d] < b | (w, _) <- G.numbers b ws, d <- w]

propNumbersComplete (Base b, Words ws) =
  let (ws', _) = unzip (G.numbers b ws)
  in  and [w `elem` ws' | w <- ws, all (\d -> fromBase b [d] < b) w]

propNumbersFrom (Base b, Words ws) =
  and [fromBase b w == n | (w, n) <- G.numbers b ws]

propGrayCodeSucc (Base b, Positive n) =
  let (_, to)    = G.grayCode b
      (ds, ds')  = pad (to (n - 1)) (to n)
  in  length (filter id $ zipWith (/=) ds ds') == 1

propGrayCodeLength (Base b, NonNegative n) =
  length (snd (G.grayCode b) n) <= 
  ceiling (log (fromIntegral n) / log (fromIntegral b)) + 1

propGrayCodeInv (Base b, NonNegative n) =
  let (from, to) = G.grayCode b
  in  from (to n) == n

propLookAndSayHead (NonNegative n) = head (G.lookAndSay n) == show n

propLookAndSayTail (NonNegative n) = test $ take 15 (G.lookAndSay n)
  where
    test (cs : css@(cs' : _)) = say (look cs) == cs' && test css
    test _                    = True

propKeithNumbersSound =
  all isKeithNumber $ take 20 (map fromIntegral G.keithGetallen)

propKeithNumbersComplete = 
  take 20 keithNumbers == take 20 (map fromIntegral G.keithGetallen)

-------------------------------------------------------------------------------
-- Batch
-------------------------------------------------------------------------------

main :: IO ()
main = mapM_ (\(l, p) -> putStr ("[" ++ l ++ "] ") >> quickCheck p) $
  [ ("fromBin               ", property propFromBin             )
  , ("toBin                 ", property propToBin               )
  , ("fromDec               ", property propFromDec             )
  , ("toDec                 ", property propToDec               )
  , ("fromBase              ", property propFromBase            )
  , ("toBase                ", property propToBase              )
  , ("numbers: sound        ", property propNumbersSound        )
  , ("numbers: complete     ", property propNumbersComplete     )
  , ("numbers: from         ", property propNumbersFrom         )
  , ("grayCode: succ        ", property propGrayCodeSucc        )
  , ("grayCode: length      ", property propGrayCodeLength      )
  , ("grayCode: inv         ", property propGrayCodeInv         ) 
  , ("lookAndSay: head      ", property propLookAndSayHead      )
  , ("lookAndSay: tail      ", property propLookAndSayTail      )
  , ("keithNumbers: sound   ", property propKeithNumbersSound   )
  , ("keithNumbers: complete", property propKeithNumbersComplete)
  ]