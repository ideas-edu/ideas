{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
-- Testing strategy combinator properties
--
-----------------------------------------------------------------------------
module Common.Strategy.Tests (tests) where

import Common.Algebra.Group
import Common.Algebra.Law
import Common.Library
import Common.Utils.TestSuite
import Control.Monad
import Data.List
import Data.Ord
import Test.QuickCheck hiding (label)
import qualified Common.Algebra.Field as F

---------------------------------------------------------
-- Properties

tests :: TestSuite
tests = suite "Strategy combinator properties" $ do
   -- monoids and semi-rings
   fs (commutative : idempotent : monoidLaws :: [Law Choice]) 
   fs (monoidZeroLaws :: [Law Sequence]) 
   fs (commutative : monoidZeroLaws :: [Law Interleave])
   fs (F.distributiveLaws :: [Law Sequence])
   fs (F.distributiveLaws :: [Law Interleave])

   -- properties of atomic
   addProperty "atomic-twice" $ \a -> 
      atomic (atomic a) === atomic (idS a)
   assertTrue  "atomic-succeed" $ 
      atomic succeed === succeed
   assertTrue  "atomic-fail" $ 
      atomic failS === failS
   addProperty "atomic-choice" $ \a b -> 
      atomic (idS a <|> idS b) === atomic a <|> atomic b

   -- splits theorm parallel/atomic
   addProperty "atomic-split"  $ \x y a b -> 
      (atomic x <*> a) <%> (atomic y <*> b) 
      === 
      (idS x <*> (a <%> (atomic y <*> b))) 
        <|>
      (idS y <*> ((atomic x <*> idS a) <%> idS b))
 where
   fs :: (Arbitrary a, Show a, Eq a) => [Law a] -> TestSuite
   fs = mapM_ (\p -> addProperty (show p) p)

-- Only a selected set of combinators is used
instance Arbitrary (Strategy a) where
   arbitrary = sized strategyGen

strategyGen :: Int -> Gen (Strategy a)
strategyGen n
   | n == 0 = frequency
        [ (1, return succeed), (1, return failS)
        , (5, liftM toStrategy ruleGen)
        ]
   | otherwise = oneof
        [ bin (<*>), bin (<|>), bin (<%>), liftM atomic rec
        , liftM (toStrategy . label "L") rec
        ]
 where
   bin f = liftM2 f rec rec
   rec   = strategyGen (n `div` 2)
        
ruleGen :: Gen (Rule a)
ruleGen = elements [ makeSimpleRule [c] Just | c <- ['A' .. 'E'] ]
   
---------------------------------------------------------
-- Algebraic instances

newtype Choice     = Choice     (Strategy ()) deriving (Show, Arbitrary)
newtype Sequence   = Sequence   (Strategy ()) deriving (Show, Arbitrary)
newtype Interleave = Interleave (Strategy ()) deriving (Show, Arbitrary)

instance Eq Choice     where     Choice a == Choice b     = a === b
instance Eq Sequence   where   Sequence a == Sequence b   = a === b
instance Eq Interleave where Interleave a == Interleave b = a === b

instance Monoid Choice where
   mempty = Choice failS
   mappend (Choice a) (Choice b) = Choice (a <|> b)

instance Monoid Sequence where
   mempty = Sequence succeed
   mappend (Sequence a) (Sequence b) = Sequence (a <*> b)
   
instance MonoidZero Sequence where
   mzero = Sequence failS

instance Monoid Interleave where
   mempty = Interleave succeed
   mappend (Interleave a) (Interleave b) = Interleave (a <%> b)
   
instance MonoidZero Interleave where
   mzero = Interleave failS
   
instance F.SemiRing Sequence where
   Sequence a <+> Sequence b = Sequence (a <|> b)
   zero  = Sequence failS
   (<*>) = mappend
   one   = mempty

instance F.SemiRing Interleave where
   Interleave a <+> Interleave b = Interleave (a <|> b)
   zero  = Interleave failS
   (<*>) = mappend
   one   = mempty

---------------------------------------------------------
-- Helper functions for equality
  
idS :: Strategy () -> Strategy ()
idS = id

infix 1 === 

(===) :: Strategy () -> Strategy () -> Bool
a === b = rec (10::Int) [(make a, make b)]
 where
   make x = restrictHeight 2 (derivationTree x ())
 
   rec _ [] = True
   rec n ((s, t):rest)
      | n == 0    = True
      | otherwise = 
           let (as, ss) = unzip (merged s)
               (bs, ts) = unzip (merged t)
           in (endpoint s, nub as) == (endpoint t, nub bs) && rec (n-1) (zip ss ts ++ rest)
      
   
merged :: Show s => DerivationTree s a -> [(s, DerivationTree s a)]
merged = map f . groupBy eq . sortBy cmp . branches
 where
   cmp a b = comparing show (fst a) (fst b)
   eq  a b = show (fst a) == show (fst b)
   f xs    = (fst $ head xs, foldr1 merge (map snd xs))
      
merge :: DerivationTree s a -> DerivationTree s a -> DerivationTree s a
merge s t = addBranches (branches s ++ branches t)   
          $ singleNode (root s) (endpoint s || endpoint t)
