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
   , smallerThan, smallerThanOrEqualTo
     -- Making intervals
   , toList, fromList, contains
   ) where

import Control.Monad

--------------------------------------------------------------------
-- Data declarations

newtype Intervals a = IS [Interval a]
   deriving Eq

data Interval a = Empty | I (Endpoint a) (Endpoint a)
   deriving Eq

data Endpoint a = Excluding a | Including a | Unbounded
   deriving Eq

instance Show a => Show (Intervals a) where
   show = show . toList

instance Show a => Show (Interval a) where
   show interval =
      case interval of
         Empty -> "{}"
         I a b -> showLeft a ++ "," ++ showRight b

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

smallerThan :: Ord a => a -> Interval a
smallerThan a = makeInterval Unbounded (Excluding a)

smallerThanOrEqualTo :: Ord a => a -> Interval a
smallerThanOrEqualTo a = makeInterval Unbounded (Including a)

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

--------------------------------------------------------------------
-- Combining multiple intervals

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
merge (I al ar) (I bl br) = do
   guard (minPoint ar bl == bl)
   guard (maxPoint al br == br)
   return (makeInterval (minPoint al bl) (maxPoint ar br))

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