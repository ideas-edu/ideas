-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Domain.Math.Data.Interval
   ( -- Data types
     Intervals, Interval
     -- Interval constructors
   , empty, singleton, unbounded, open, closed
   , leftOpen, rightOpen, greaterThan, greaterThanOrEqualTo
   , lessThan, lessThanOrEqualTo
     -- Making intervals
   , except, toList, fromList, contains
   , union, intersect, complement
   ) where

import Common.Utils (commaList)
import Control.Monad
import Data.Maybe

--------------------------------------------------------------------
-- Data declarations

newtype Intervals a = IS [Interval a]
   deriving Eq

data Interval a = Empty | I (Endpoint a) (Endpoint a)
   deriving Eq

data Endpoint a = Excluding a | Including a | Unbounded
   deriving Eq

instance Show a => Show (Intervals a) where
   show xs = "{ " ++ commaList (map show (toList xs)) ++ " }"

instance Show a => Show (Interval a) where
   show interval =
      case interval of
         Empty -> "{}"
         I a b -> showLeft a ++ "," ++ showRight b

instance Functor Endpoint where
   fmap f (Excluding a) = Excluding (f a)
   fmap f (Including a) = Including (f a)
   fmap f Unbounded     = Unbounded

instance Functor Interval where
   fmap f Empty   = Empty
   fmap f (I a b) = I (fmap f a) (fmap f b) -- function should not change order

instance Functor Intervals where
   fmap f (IS xs) = IS (map (fmap f) xs)

showLeft, showRight :: Show a => Endpoint a -> String
showLeft  (Excluding a) = "(" ++ show a
showLeft  (Including a) = "[" ++ show a
showLeft  Unbounded     = "(-inf"
showRight (Excluding a) = show a ++ ")"
showRight (Including a) = show a ++ "]"
showRight Unbounded     = "inf)"

--------------------------------------------------------------------
-- Interval constructors

empty :: Interval a
empty = Empty

singleton :: Ord a => a -> Interval a
singleton a = closed a a

unbounded :: Ord a => Interval a
unbounded = makeInterval Unbounded Unbounded

open :: Ord a => a -> a -> Interval a
open a b = makeInterval (Excluding a) (Excluding b)

closed :: Ord a => a -> a -> Interval a
closed a b = makeInterval (Including a) (Including b)

leftOpen :: Ord a => a -> a -> Interval a
leftOpen a b = makeInterval (Excluding a) (Including b)

rightOpen :: Ord a => a -> a -> Interval a
rightOpen a b = makeInterval (Including a) (Excluding b)

greaterThan :: Ord a => a -> Interval a
greaterThan a = makeInterval (Excluding a) Unbounded

greaterThanOrEqualTo :: Ord a => a -> Interval a
greaterThanOrEqualTo a = makeInterval (Including a) Unbounded

lessThan :: Ord a => a -> Interval a
lessThan a = makeInterval Unbounded (Excluding a)

lessThanOrEqualTo :: Ord a => a -> Interval a
lessThanOrEqualTo a = makeInterval Unbounded (Including a)

-- local constructor
makeInterval :: Ord a => Endpoint a -> Endpoint a -> Interval a
makeInterval pl pr =
   case liftM2 compare (getPoint pl) (getPoint pr) of
      Just LT -> I pl pr
      Just EQ
         | isIncluding pl && isIncluding pr -> I pl pr
         | otherwise                        -> Empty
      Just GT -> Empty
      Nothing -> I pl pr

isIncluding :: Endpoint a -> Bool
isIncluding (Including _) = True
isIncluding _             = False

isExcluding :: Endpoint a -> Bool
isExcluding (Excluding _) = True
isExcluding _             = False

--------------------------------------------------------------------
-- Combining multiple intervals

except :: Ord a => a -> Intervals a
except a = fromList [lessThan a, greaterThan a]

toList :: Intervals a -> [Interval a]
toList (IS xs) = xs

fromList :: Ord a => [Interval a] -> Intervals a
fromList = foldr insert (IS [])

insert :: Ord a => Interval a -> Intervals a -> Intervals a
insert Empty xs  = xs
insert iv@(I l _) (IS xs) = rec xs
 where
   rec []        = IS [iv]
   rec (hd:rest) =
      case (hd, merge iv hd) of
         (Empty, _)       -> rec rest
         (_, Just new)    -> insert new (IS rest)
         (I a _, Nothing)
            | minPoint a l == a -> let IS tl = rec rest in IS (hd:tl)
            | otherwise         -> IS (iv:hd:rest)

contains :: Ord a => Interval a -> Interval a -> Bool
contains _ Empty = True
contains Empty _ = False
contains (I al ar) (I bl br) = 
   minPoint al bl == al && maxPoint ar br == ar

union :: Ord a => Intervals a -> Intervals a -> Intervals a
union xs = foldr insert xs . toList

intersect :: Ord a => Intervals a -> Intervals a -> Intervals a
intersect (IS xs) (IS ys) = fromList (f xs ys)
 where
   f (a@(I _ ar):as) (b@(I _ br):bs) = inBoth a b : rest
    where
      rest | maxPoint ar br == ar = f (a:as) bs
           | otherwise            = f as (b:bs)
   f _ _ = []

complement :: Ord a => Intervals a -> Intervals a
complement (IS xs) = fromList (left ++ zipWith f xs (drop 1 xs) ++ right)
 where
   f (I _ a) (I b _) = fromMaybe Empty (liftM2 I (g a) (g b))
   f _ _             = Empty
   
   g (Including a) = Just (Excluding a)
   g (Excluding a) = Just (Including a)
   g Unbounded     = Nothing
   
   left = case xs of 
             I al _:_ -> maybe [] (return . I Unbounded) (g al)
             _        -> [unbounded]
   right = case reverse xs of 
              I _ ar:_ -> maybe [] (return . flip I Unbounded) (g ar)
              _        -> [unbounded]
 
-- q = intersect (fromList [open 1 8]) (fromList [closed 1 4, open 5 6, open 6 7])
q = fromList [greaterThan (-4)] `intersect` fromList [lessThan 1]

---------------------------------------------------------------------
-- Local helper functions
{-
leftEndpoint, rightEndpoint :: Interval a -> Maybe a
leftEndpoint  Empty    = Nothing
leftEndpoint  (I pl _) = getPoint pl
rightEndpoint Empty    = Nothing
rightEndpoint (I _ pr) = getPoint pr -}

getPoint :: Endpoint a -> Maybe a
getPoint (Including a) = Just a
getPoint (Excluding a) = Just a
getPoint Unbounded     = Nothing

merge :: Ord a => Interval a -> Interval a -> Maybe (Interval a)
merge a Empty = Just a
merge Empty b = Just b
merge ia@(I al ar) ib@(I bl br)
   | minPoint al bl /= al = merge ib ia
   | otherwise = 
        case liftM2 compare (getPoint ar) (getPoint bl) of
           Just LT -> Nothing
           Just EQ
              | isIncluding ar || isIncluding bl -> ok
              | otherwise     -> Nothing
           Just GT -> ok
           Nothing -> ok
 where
   ok = Just (I al (maxPoint ar br))

inBoth :: Ord a => Interval a -> Interval a -> Interval a
inBoth a Empty = Empty
inBoth Empty b = Empty
inBoth (I al ar) (I bl br) = makeInterval (maxPoint3 al bl) (minPoint3 ar br)

minPoint :: Ord a => Endpoint a -> Endpoint a -> Endpoint a
minPoint a b = 
   case liftM2 compare (getPoint a) (getPoint b) of
      Just LT -> a
      Just EQ
         | isIncluding a -> a
         | otherwise     -> b
      Just GT -> b
      Nothing -> Unbounded

maxPoint :: Ord a => Endpoint a -> Endpoint a -> Endpoint a
maxPoint a b = 
   case liftM2 compare (getPoint a) (getPoint b) of
      Just LT -> b
      Just EQ
         | isIncluding a -> a
         | otherwise     -> b
      Just GT -> a
      Nothing -> Unbounded
      
minPoint2 :: Ord a => Endpoint a -> Endpoint a -> Endpoint a
minPoint2 a b = 
   case liftM2 compare (getPoint a) (getPoint b) of
      Just LT -> a
      Just EQ
         | isIncluding a -> b -- !!!!!!!!!!!!!!!!
         | otherwise     -> a
      Just GT -> b
      Nothing -> Unbounded
      
maxPoint2 :: Ord a => Endpoint a -> Endpoint a -> Endpoint a
maxPoint2 a b = 
   case liftM2 compare (getPoint a) (getPoint b) of
      Just LT -> b
      Just EQ
         | isIncluding a -> b -- !!!
         | otherwise     -> a
      Just GT -> a
      Nothing -> Unbounded
      
maxPoint3 :: Ord a => Endpoint a -> Endpoint a -> Endpoint a
maxPoint3 a b = 
   case liftM2 compare (getPoint a) (getPoint b) of
      Just LT -> b
      Just EQ
         | isIncluding a -> b -- !!!
         | otherwise     -> a
      Just GT -> a
      Nothing -> if a == Unbounded then b else a
      
minPoint3 :: Ord a => Endpoint a -> Endpoint a -> Endpoint a
minPoint3 a b = 
   case liftM2 compare (getPoint a) (getPoint b) of
      Just LT -> a
      Just EQ
         | isIncluding a -> b -- !!!!!!!!!!!!!!!!
         | otherwise     -> a
      Just GT -> b
      Nothing -> if a == Unbounded then b else a