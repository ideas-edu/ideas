{-# LANGUAGE ExistentialQuantification #-}
-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- A collection of general utility functions
--
-----------------------------------------------------------------------------
module Common.Utils where

import Control.Monad
import Data.Char
import Data.List
import Data.Ratio
import System.Random
import Test.QuickCheck

data Some f = forall a . Some (f a)

data ShowString = ShowString { fromShowString :: String }
   deriving (Eq, Ord)

instance Show ShowString where
   show = fromShowString

readInt :: String -> Maybe Int
readInt xs 
   | null xs                = Nothing
   | any (not . isDigit) xs = Nothing
   | otherwise              = Just (foldl' (\a b -> a*10+ord b-48) 0 xs) -- '

readM :: (Monad m, Read a) => String -> m a
readM s = case reads s of
             [(a, xs)] | all isSpace xs -> return a
             _ -> fail ("no read: " ++ s)

stringToHex :: String -> Maybe Int
stringToHex = foldl op (Just 0)
 where
   op (Just i) c = fmap (i*16+) (charToHex c)
   op Nothing  _ = Nothing

charToHex :: Char -> Maybe Int
charToHex c
   | isDigit c = return (ord c - 48)
   | toUpper c `elem` ['A' .. 'F'] = return (ord (toUpper c) - 55)
   | otherwise = Nothing

subsets :: [a] -> [[a]]
subsets = foldr op [[]]
 where op a list = list ++ map (a:) list
 
isSubsetOf :: Eq a => [a] -> [a] -> Bool
isSubsetOf xs ys = all (`elem` ys) xs

cartesian :: [a] -> [b] -> [(a, b)]
cartesian as bs = [ (a, b) | a <- as, b <- bs ]

distinct :: Eq a => [a] -> Bool
distinct []     = True
distinct (x:xs) = all (/=x) xs && distinct xs 

allsame :: Eq a => [a] -> Bool
allsame []     = True
allsame (x:xs) = all (==x) xs

safeHead :: [a] -> Maybe a
safeHead (x:_) = return x
safeHead _     = Nothing

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f = stop . iterate f 
 where
   stop []           = error "Common.Utils: empty list"
   stop (x:xs)
      | x == head xs = x
      | otherwise    = stop xs
      
fixpointM :: (Monad m, Eq a) => (a -> m a) -> a -> m a
fixpointM f a = do
   b <- f a
   if a==b then return a else fixpointM f b
   
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

splitAtSequence :: Eq a => [a] -> [a] -> Maybe ([a], [a])
splitAtSequence cs = f []
 where
   f _   [] = Nothing
   f acc list@(x:xs)
      | cs `isPrefixOf` list = Just (reverse acc, drop (length cs) list)
      | otherwise            = f (x:acc) xs

-- | Use a fixed standard "random" number generator. This generator is
-- accessible by calling System.Random.getStdGen
useFixedStdGen :: IO ()
useFixedStdGen = setStdGen (mkStdGen 280578) {- magic number -}

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

thd3 :: (a, b, c) -> c
thd3 (_, _, x) = x

commaList :: [String] -> String
commaList = concat . intersperse ", "

-- | Prevents a bias towards small numbers
ratioGen :: Integral a => Int -> Int -> Gen (Ratio a)
ratioGen n m = do 
   a <- choose (-n, n)
   b <- liftM (succ . abs) (choose (-m, m))
   c <- choose (1-b, b-1)
   return (fromIntegral a + (fromIntegral c / fromIntegral b))
   
swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)