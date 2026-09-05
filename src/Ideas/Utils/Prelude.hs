{-# LANGUAGE ExistentialQuantification #-}
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
-- A collection of general utility functions
--
-----------------------------------------------------------------------------

module Ideas.Utils.Prelude
   ( Some(..), ShowString(..), readInt, readM
   , subsets, isSubsetOf
   , cartesian, distinct, allsame
   , fixpoint
   , split, split3, split4
   , splitAtElem, splitsWithElem
   , timedSeconds, getDiffTime
   , fst3, snd3, thd3
   , mwhen, munless
   ) where

import Data.Char
import Data.List
import Data.Time
import System.Timeout

data Some f = forall a . Some (f a)

newtype ShowString = ShowString { fromShowString :: String }
   deriving (Eq, Ord)

instance Show ShowString where
   show = fromShowString

instance Read ShowString where
   readsPrec n s = [ (ShowString x, y) | (x, y) <- readsPrec n s ]

readInt :: String -> Maybe Int
readInt xs
   | null xs              = Nothing
   | not (all isDigit xs) = Nothing
   | otherwise            = Just (foldl' (\a b -> a*10+ord b-48) 0 xs) -- '

{-# INLINE readM #-}
readM :: Read a => String -> Maybe a
readM s = case reads s of
             [(a, xs)] | all isSpace xs -> Just a
             _ -> Nothing

subsets :: [a] -> [[a]]
subsets = foldr op [[]]
 where op a xs = xs ++ map (a:) xs

isSubsetOf :: Eq a => [a] -> [a] -> Bool
isSubsetOf xs ys = all (`elem` ys) xs

cartesian :: [a] -> [b] -> [(a, b)]
cartesian as bs = [ (a, b) | a <- as, b <- bs ]

distinct :: Eq a => [a] -> Bool
distinct []     = True
distinct (x:xs) = notElem x xs && distinct xs

allsame :: Eq a => [a] -> Bool
allsame []     = True
allsame (x:xs) = all (==x) xs

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f = rec . iterate f
 where
   rec [] = error "Ideas.Common.Utils: empty list"
   rec (x:xs)
      | x == head xs = x
      | otherwise    = rec xs

split :: [a] -> [([a], [a])]
split xs = map (`splitAt` xs) [0 .. length xs]  

split3 :: [a] -> [([a], [a], [a])]
split3 as = 
   [ (xs, ys1, ys2)
   | (xs, ys) <- split as
   , (ys1, ys2) <- split ys
   ]

split4 :: [a] -> [([a], [a], [a], [a])]
split4 as =
   [ (xs1, xs2, ys1, ys2) 
   | (xs, ys) <- split as
   , (xs1, xs2) <- split xs
   , (ys1, ys2) <- split ys
   ]

splitAtElem :: Eq a => a -> [a] -> Maybe ([a], [a])
splitAtElem c s =
   case break (==c) s of
      (xs, _:ys) -> Just (xs, ys)
      _          -> Nothing

splitsWithElem :: Eq a => a -> [a] -> [[a]]
splitsWithElem c s =
   case splitAtElem c s of
      Just (xs, ys) -> xs : splitsWithElem c ys
      Nothing       -> [s]

timedSeconds :: Int -> IO a -> IO a
timedSeconds n m = timeout (n * 10^(6 :: Int)) m >>=
   maybe (fail ("Timeout after " ++ show n ++ " seconds")) return

getDiffTime :: IO a -> IO (a, NominalDiffTime)
getDiffTime action = do
   t0 <- getCurrentTime
   a  <- action
   t1 <- getCurrentTime
   return (a, diffUTCTime t1 t0)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

thd3 :: (a, b, c) -> c
thd3 (_, _, x) = x

-- Monoids

mwhen :: Monoid a => Bool -> a -> a
mwhen True  a = a
mwhen False _ = mempty

munless :: Monoid a => Bool -> a -> a
munless = mwhen . not