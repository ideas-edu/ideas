-----------------------------------------------------------------------------
-- Copyright 2019, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Support for the UTF8 encoding
--
-----------------------------------------------------------------------------

module Ideas.Text.UTF8
   ( encode, decode
   , isUTF8, allBytes, propEncoding
   ) where

import Control.Monad
import Data.Char
import Data.Maybe
import Test.QuickCheck

------------------------------------------------------------------
-- Interface

-- | Encode a string to UTF8 format
encode :: String -> Maybe String
encode = fmap (map chr . concat) . mapM (toUTF8 . ord)

-- | Decode an UTF8 format string to unicode points
decode :: String -> Maybe String
decode = fmap (map chr) . fromUTF8 . map ord

-- | Test whether the argument is a proper UTF8 string
isUTF8 :: String -> Bool
isUTF8 = isJust . decode

-- | Test whether all characters are in the range 0-255
allBytes :: String -> Bool
allBytes = all ((`between` (0, 255)) . ord)

------------------------------------------------------------------
-- Helper functions

toUTF8 :: Int -> Maybe [Int]
toUTF8 n
   | n < 128 = -- one byte
        Just [n]
   | n < 2048 = -- two bytes
        let (a, d) = n `divMod` 64
        in Just [a+192, d+128]
   | n < 65536 = -- three bytes
        let (a, d1) = n  `divMod` 4096
            (b, d2) = d1 `divMod` 64
        in Just [a+224, b+128, d2+128]
   | n < 1114112 = -- four bytes
        let (a, d1) = n  `divMod` 262144
            (b, d2) = d1 `divMod` 4096
            (c, d3) = d2 `divMod` 64
        in Just [a+240, b+128, c+128, d3+128]
   | otherwise =
        Nothing

fromUTF8 :: [Int] -> Maybe [Int]
fromUTF8 xs
   | null xs   = return []
   | otherwise = do
        (i, rest) <- f xs
        is <- fromUTF8 rest
        return (i:is)
 where
   f (a:rest) | a < 128 = -- one byte
      Just (a, rest)
   f (a:b:rest) | a `between` (192, 223) = -- two bytes
      checked (isHigh b)
         ((a-192)*64 + b-128, rest)
   f (a:b:c:rest) | a `between` (224, 239) = -- three bytes
      checked (isHigh b && isHigh c)
         ((a-224)*4096 + (b-128)*64 + c-128, rest)
   f (a:b:c:d:rest) | a >= 240 && a < 248 = -- four bytes
      let value = (a-240)*262144 + (b-128)*4096 + (c-128)*64 + d-128
      in checked (isHigh b && isHigh c && isHigh d && value <= 1114111) (value, rest)
   f _ = Nothing

checked :: Bool -> a -> Maybe a
checked b a = if b then Just a else Nothing

isHigh :: Int -> Bool
isHigh i = i `between` (128, 191)

between :: Ord a => a -> (a, a) -> Bool
between a (low, high) = low <= a && a <= high

------------------------------------------------------------------
-- Test encoding

-- | QuickCheck internal encoding/decoding functions
propEncoding :: Property
propEncoding = forAll (sized gen) valid
 where
   gen n = replicateM n someChar
   someChar = chr <$> oneof
      -- To get a nice distribution over the number of bytes used
      -- in the encoding
      [ choose (0, 127), choose (128, 2047)
      , choose (2048, 65535), choose (65536, 1114111)
      ]

valid :: String -> Bool
valid xs = fromMaybe False $
   do us <- encode xs
      bs <- decode us
      return (xs == bs)