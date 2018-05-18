{-# LANGUAGE ExistentialQuantification #-}
-----------------------------------------------------------------------------
-- Copyright 2018, Ideas project team. This file is distributed under the
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
   , splitAtElem, splitsWithElem
   , timedSeconds
   , fst3, snd3, thd3
   , headM, findIndexM
   , elementAt, changeAt, replaceAt
   , list
   ) where

import Data.Char
import Data.List
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
   | null xs                = Nothing
   | any (not . isDigit) xs = Nothing
   | otherwise              = Just (foldl' (\a b -> a*10+ord b-48) 0 xs) -- '

readM :: (Monad m, Read a) => String -> m a
readM s = case reads s of
             [(a, xs)] | all isSpace xs -> return a
             _ -> fail ("no read: " ++ s)

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

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

thd3 :: (a, b, c) -> c
thd3 (_, _, x) = x

-- generalized list functions (results in monad)
headM :: Monad m => [a] -> m a
headM (a:_) = return a
headM _     = fail "headM"

findIndexM :: Monad m => (a -> Bool) -> [a] -> m Int
findIndexM p = maybe (fail "findIndexM") return . findIndex p

elementAt :: Monad m => Int -> [a] -> m a
elementAt i = headM . drop i

changeAt :: Monad m => Int -> (a -> a) -> [a] -> m [a]
changeAt i f as =
   case splitAt i as of
      (xs, y:ys) -> return (xs ++ f y : ys)
      _          -> fail "changeAt"

replaceAt :: Monad m => Int -> a -> [a] -> m [a]
replaceAt i = changeAt i . const

list :: b -> ([a] -> b) -> [a] -> b
list b f xs = if null xs then b else f xs