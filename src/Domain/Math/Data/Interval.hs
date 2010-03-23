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
-- Support for mathematical intervals (open, closed, unbounded). @Intervals@
-- is a normalized (and sorted) list of intervals that supports testing for
-- equality.
--
-----------------------------------------------------------------------------
module Domain.Math.Data.Interval
   ( -- Data types
     Intervals, Interval
     -- Interval constructors
   , empty, singleton, unbounded, open, closed
   , leftOpen, rightOpen, greaterThan, greaterThanOrEqualTo
   , lessThan, lessThanOrEqualTo
     -- Inspecing an interval
   , isEmpty, leftPoint, rightPoint, Endpoint(..)
     -- Making intervals
   , except, toList, fromList
   , union, intersect, complement
   , isIn, isInInterval
     -- QuickChecks
   , testMe
   ) where

import Common.Utils (commaList)
import Control.Monad
import Data.Maybe
import Test.QuickCheck

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
   fmap _ Unbounded     = Unbounded

instance Functor Interval where
   fmap _ Empty   = Empty
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
-- Inspecting an interval

isEmpty :: Interval a -> Bool
isEmpty Empty = True
isEmpty _     = False

leftPoint, rightPoint :: Interval a -> Endpoint a
leftPoint  (I a _) = a
leftPoint Empty    = error "leftPoint Empty"
rightPoint (I _ a) = a
rightPoint Empty   = error "rightPoint Empty"

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
            | minPointLeft a l == a -> let IS tl = rec rest in IS (hd:tl)
            | otherwise             -> IS (iv:hd:rest)

union :: Ord a => Intervals a -> Intervals a -> Intervals a
union xs = foldr insert xs . toList

intersect :: Ord a => Intervals a -> Intervals a -> Intervals a
intersect (IS xs) (IS ys) = fromList (f xs ys)
 where
   f (a@(I _ ar):as) (b@(I _ br):bs) = inBoth a b : rest
    where
      rest | maxPointRight ar br == ar = f (a:as) bs
           | otherwise                 = f as (b:bs)
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

isIn :: Ord a => a -> Intervals a -> Bool
isIn a (IS xs) = any (isInInterval a) xs

isInInterval :: Ord a => a -> Interval a -> Bool
isInInterval _ Empty   = False
isInInterval a (I b c) = f GT b && f LT c
 where
   f value x = 
      let g c = (c==EQ && isIncluding x) || c==value 
      in maybe True (g . compare a) (getPoint x)

---------------------------------------------------------------------
-- Local helper functions

getPoint :: Endpoint a -> Maybe a
getPoint (Including a) = Just a
getPoint (Excluding a) = Just a
getPoint Unbounded     = Nothing

merge :: Ord a => Interval a -> Interval a -> Maybe (Interval a)
merge a Empty = Just a
merge Empty b = Just b
merge ia@(I al ar) ib@(I bl br)
   | minPointLeft al bl /= al = merge ib ia
   | otherwise = 
        case liftM2 compare (getPoint ar) (getPoint bl) of
           Just LT -> Nothing
           Just EQ
              | isIncluding ar || isIncluding bl -> ok
              | otherwise     -> Nothing
           Just GT -> ok
           Nothing -> ok
 where
   ok = Just (I al (maxPointRight ar br))

inBoth :: Ord a => Interval a -> Interval a -> Interval a
inBoth _ Empty = Empty
inBoth Empty _ = Empty
inBoth (I al ar) (I bl br) = makeInterval (maxPointLeft al bl) (minPointRight ar br)

minPointLeft, minPointRight, maxPointLeft, maxPointRight 
   :: Ord a => Endpoint a -> Endpoint a -> Endpoint a
minPointLeft  = compareEndpoint True  True
minPointRight = compareEndpoint True  False 
maxPointLeft  = compareEndpoint False False
maxPointRight = compareEndpoint False True 

compareEndpoint :: Ord a => Bool -> Bool -> Endpoint a -> Endpoint a -> Endpoint a
compareEndpoint b1 b2 a b = 
   case liftM2 compare (getPoint a) (getPoint b) of
      Just LT                -> x
      Just EQ | p a          -> x 
              | otherwise    -> y
      Just GT                -> y
      Nothing | b2           -> Unbounded
              | x==Unbounded -> y
              | otherwise    -> x
 where
   p = if b1==b2 then isIncluding else isExcluding
   (x, y) = if b1 then (a, b) else (b, a)
  
---------------------------------------------------------------------
-- QuickCheck

instance (Arbitrary a, Ord a) => Arbitrary (Endpoint a) where
   arbitrary = frequency 
      [ (2, liftM Excluding arbitrary)
      , (2, liftM Including arbitrary)
      , (1, return Unbounded)
      ]
   coarbitrary (Excluding a) = variant 0 . coarbitrary a
   coarbitrary (Including a) = variant 1 . coarbitrary a
   coarbitrary Unbounded     = variant 2

instance (Arbitrary a, Ord a) => Arbitrary (Interval a) where
   arbitrary = frequency 
      [ (1, return Empty)
      , (5, liftM2 makeInterval arbitrary arbitrary)
      ]
   coarbitrary Empty   = variant 0
   coarbitrary (I a b) = variant 1 . coarbitrary a . coarbitrary b
   
instance (Arbitrary a, Ord a) => Arbitrary (Intervals a) where
   arbitrary = do
      n  <- choose (0, 100)
      xs <- replicateM n arbitrary
      return (fromList xs)
   coarbitrary (IS xs) = coarbitrary xs

testMe :: IO ()
testMe = do
   putStrLn "** Intervals"
   -- Constructor functions
   quickCheck $ op0 empty     (const False)
   quickCheck $ op0 unbounded (const True)
   
   quickCheck $ op1 greaterThan (>)
   quickCheck $ op1 greaterThanOrEqualTo (>=)
   quickCheck $ op1 lessThan (<)
   quickCheck $ op1 lessThanOrEqualTo (<=)
   quickCheck $ op1 singleton (==)
   
   quickCheck $ op2 open      (<)  (<)
   quickCheck $ op2 closed    (<=) (<=)
   quickCheck $ op2 leftOpen  (<)  (<=)
   quickCheck $ op2 rightOpen (<=) (<)
   
   -- From/to lists
   quickCheck fromTo1
   quickCheck fromTo2
   
   -- Combinators
   quickCheck defExcept
   quickCheck defUnion
   quickCheck defIntersect
   quickCheck defComplement
   
   -- Combinator properties
   quickCheck $ selfInverse complement
   quickCheck $ transitive  union
   quickCheck $ commutative union
   quickCheck $ absorption  union
   quickCheck $ transitive  intersect
   quickCheck $ commutative intersect
   quickCheck $ absorption  intersect

fromTo1, fromTo2 :: Intervals Int -> Bool
fromTo1 a = fromList (toList a) == a
fromTo2 a = fromList (reverse (toList a)) == a

defExcept :: Int -> Int -> Bool
defExcept a b = isIn a (except b) == (a/=b)

defUnion, defIntersect :: Int -> Intervals Int -> Intervals Int -> Bool
defUnion     a b c = isIn a (b `union` c) == (isIn a b || isIn a c)
defIntersect a b c = isIn a (b `intersect` c) == (isIn a b && isIn a c)

defComplement :: Int -> Intervals Int -> Bool
defComplement a b = isIn a (complement b) == not (isIn a b)

op0 :: Interval Int -> (Int -> Bool) -> Int -> Bool
op0 g p a = isInInterval a g == p a

op1 :: (Int -> Interval Int) -> (Int -> Int -> Bool) -> Int -> Int -> Bool
op1 g op a b = isInInterval a (g b) == (a `op` b)

op2 :: (Int -> Int -> Interval Int) -> (Int -> Int -> Bool) -> (Int -> Int -> Bool) -> Int -> Int -> Int -> Bool
op2 g opl opr a b c = isInInterval a (g b c) == (b `opl` a && a `opr` c)

transitive :: (Intervals Int -> Intervals Int -> Intervals Int) -> Intervals Int -> Intervals Int -> Intervals Int -> Bool
transitive op a b c = op a (op b c) == op (op a b) c

commutative :: (Intervals Int -> Intervals Int -> Intervals Int) -> Intervals Int -> Intervals Int -> Bool
commutative op a b = op a b == op b a

absorption :: (Intervals Int -> Intervals Int -> Intervals Int) -> Intervals Int -> Bool
absorption op a = op a a == a

selfInverse :: (Intervals Int -> Intervals Int) -> Intervals Int -> Bool
selfInverse op a = op (op a) == a