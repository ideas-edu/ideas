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
-- Support for mathematical intervals (open, closed, unbounded). @Intervals@
-- is a normalized (and sorted) list of intervals that supports testing for
-- equality.
--
-----------------------------------------------------------------------------
module Domain.Math.Data.Interval
   ( -- Data types
     Interval
     -- Interval constructors
   , empty, point, unbounded, open, closed
   , leftOpen, rightOpen, greaterThan, greaterThanOrEqualTo
   , lessThan, lessThanOrEqualTo
     -- Inspecing an interval
   , Endpoint(..), segments
     -- Making intervals
   , except
   , union, intersect, complement
   , isIn, true, false
     -- QuickChecks
   , testMe
   ) where

import Common.Algebra.Boolean
import Common.Algebra.Law
import Common.TestSuite
import Common.Utils (commaList)
import Control.Monad
import Data.Maybe
import Test.QuickCheck

--------------------------------------------------------------------
-- Data declarations

newtype Interval a = I [Segment a]
   deriving Eq

data Segment a = S (Endpoint a) (Endpoint a)
   deriving Eq

data Endpoint a = Excluding a | Including a | Unbounded
   deriving Eq

instance Ord a => BoolValue (Interval a) where
   fromBool b = if b then unbounded else empty
   isTrue   = (==true)
   isFalse  = (==false)

instance Ord a => Boolean (Interval a) where
   (<&&>)     = intersect
   (<||>)     = union
   complement = complementIntervals

instance Show a => Show (Interval a) where
   show (I xs) = "{ " ++ commaList (map show xs) ++ " }"

instance Show a => Show (Segment a) where
   show (S a b) = showLeft a ++ "," ++ showRight b

instance Functor Endpoint where
   fmap f (Excluding a) = Excluding (f a)
   fmap f (Including a) = Including (f a)
   fmap _ Unbounded     = Unbounded

showLeft, showRight :: Show a => Endpoint a -> String
showLeft  (Excluding a) = '(' : show a
showLeft  (Including a) = '[' : show a
showLeft  Unbounded     = "(-inf"
showRight (Excluding a) = show a ++ ")"
showRight (Including a) = show a ++ "]"
showRight Unbounded     = "inf)"

--------------------------------------------------------------------
-- Interval constructors

empty :: Interval a
empty = I []

point :: a -> Interval a
point a = I [S (Including a) (Including a)]

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
makeInterval pl pr = maybe empty (I . return) (makeSegment pl pr)

makeSegment :: Ord a => Endpoint a -> Endpoint a -> Maybe (Segment a)
makeSegment pl pr =
   case liftM2 compare (getPoint pl) (getPoint pr) of
      Just EQ 
         | isExcluding pl -> Nothing
         | isExcluding pr -> Nothing
      Just GT             -> Nothing
      _ -> Just (S pl pr)

isIncluding :: Endpoint a -> Bool
isIncluding (Including _) = True
isIncluding _             = False

isExcluding :: Endpoint a -> Bool
isExcluding (Excluding _) = True
isExcluding _             = False

--------------------------------------------------------------------
-- Inspecting an interval

segments :: Interval a -> [(Endpoint a, Endpoint a)]
segments (I xs) = [ (a, b) | S a b <- xs ]

--------------------------------------------------------------------
-- Combining multiple intervals

except :: Ord a => a -> Interval a
except a = lessThan a <||> greaterThan a

insert :: Ord a => Segment a -> Interval a -> Interval a
insert ia (I xs) = I (rec ia xs)
 where
   rec iv [] = [iv]
   rec iv@(S a _) (hd@(S b _):rest) =
      case merge iv hd of
         Just new -> rec new rest
         Nothing
            | minPointLeft b a == b -> hd:rec iv rest
            | otherwise             -> iv:hd:rest

union :: Ord a => Interval a -> Interval a -> Interval a
union xs (I ys) = foldr insert xs ys

intersect :: Ord a => Interval a -> Interval a -> Interval a
intersect (I xs) (I ys) = I (f xs ys)
 where
   f (a@(S _ ar):as) (b@(S _ br):bs) = 
      let cond = maxPointRight ar br == ar
          rest | cond      = f (a:as) bs
               | otherwise = f as (b:bs)
      in maybe id (:) (inBoth a b) rest
   f _ _ = []

complementIntervals :: Ord a => Interval a -> Interval a
complementIntervals (I xs) 
   | null xs   = unbounded
   | otherwise = I $ catMaybes $ 
        left (head xs) : zipWith f xs (drop 1 xs) ++ [right (last xs)]
 where
   f (S _ a) (S b _) = liftM2 S (g a) (g b)
   
   g (Including a) = Just (Excluding a)
   g (Excluding a) = Just (Including a)
   g Unbounded     = Nothing
   
   left  (S al _) = fmap (S Unbounded) (g al)
   right (S _ ar) = fmap (flip S Unbounded) (g ar)

isIn :: Ord a => a -> Interval a -> Bool
isIn a (I xs) = any p xs
 where
   p (S x y) = f GT x && f LT y
   f value b = 
      let g c = (c==EQ && isIncluding b) || c==value 
      in maybe True (g . compare a) (getPoint b)

---------------------------------------------------------------------
-- Local helper functions

getPoint :: Endpoint a -> Maybe a
getPoint (Including a) = Just a
getPoint (Excluding a) = Just a
getPoint Unbounded     = Nothing

merge :: Ord a => Segment a -> Segment a -> Maybe (Segment a)
merge ia@(S al ar) ib@(S bl br)
   | minPointLeft al bl /= al = merge ib ia
   | otherwise = 
        case liftM2 compare (getPoint ar) (getPoint bl) of
           Just LT -> Nothing
           Just EQ | isExcluding ar && isExcluding bl -> Nothing
           _ -> Just (S al (maxPointRight ar br))

inBoth :: Ord a => Segment a -> Segment a -> Maybe (Segment a)
inBoth (S al ar) (S bl br) = 
   makeSegment (maxPointLeft al bl) (minPointRight ar br)

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
instance (CoArbitrary a, Ord a) => CoArbitrary (Endpoint a) where
   coarbitrary (Excluding a) = variant (0 :: Int) . coarbitrary a
   coarbitrary (Including a) = variant (1 :: Int) . coarbitrary a
   coarbitrary Unbounded     = variant (2 :: Int)

instance (Arbitrary a, Ord a) => Arbitrary (Interval a) where
   arbitrary = do
      n  <- choose (0, 100)
      xs <- replicateM n (liftM2 makeInterval arbitrary arbitrary)
      return (ors xs)

instance (CoArbitrary a, Ord a) => CoArbitrary (Segment a) where
   coarbitrary (S a b) = coarbitrary a . coarbitrary b

instance (CoArbitrary a, Ord a) => CoArbitrary (Interval a) where
   coarbitrary (I xs) = coarbitrary xs

testMe :: TestSuite
testMe = suite "Intervals" $ do

   suite "Constructor functions" $ do
     addProperty "empty"     $ op0 empty     (const False)
     addProperty "unbounded" $ op0 unbounded (const True)
   
     addProperty "greater than"             $ op1 greaterThan (>)
     addProperty "greater than or equal to" $ op1 greaterThanOrEqualTo (>=)
     addProperty "less than"                $ op1 lessThan (<)
     addProperty "less than or equal to"    $ op1 lessThanOrEqualTo (<=)
     addProperty "point    "                $ op1 point (==)
   
     addProperty "open"       $ op2 open      (<)  (<)
     addProperty "closed"     $ op2 closed    (<=) (<=)
     addProperty "left open"  $ op2 leftOpen  (<)  (<=)
     addProperty "right open" $ op2 rightOpen (<=) (<)
   
   suite "Combinators" $ do
      addProperty "except"     defExcept
      addProperty "union"      defUnion
      addProperty "intersect"  defIntersect
      addProperty "complement" defComplement
   
   suite "Boolean algebra" $
      forM_ (booleanLaws :: [Law (Interval Int)]) $ \p ->
         addProperty (show p) p

defExcept :: Int -> Int -> Bool
defExcept a b = isIn a (except b) == (a/=b)

defUnion, defIntersect :: Int -> Interval Int -> Interval Int -> Bool
defUnion     a b c = isIn a (b `union` c) == (isIn a b || isIn a c)
defIntersect a b c = isIn a (b `intersect` c) == (isIn a b && isIn a c)

defComplement :: Int -> Interval Int -> Bool
defComplement a b = isIn a (complement b) == not (isIn a b)

op0 :: Interval Int -> (Int -> Bool) -> Int -> Bool
op0 g p a = isIn a g == p a

op1 :: (Int -> Interval Int) -> (Int -> Int -> Bool) -> Int -> Int -> Bool
op1 g op a b = isIn a (g b) == (a `op` b)

op2 :: (Int -> Int -> Interval Int) -> (Int -> Int -> Bool) -> (Int -> Int -> Bool) -> Int -> Int -> Int -> Bool
op2 g opl opr a b c = isIn a (g b c) == (b `opl` a && a `opr` c)