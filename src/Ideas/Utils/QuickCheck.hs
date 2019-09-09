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
-- Extensions to the QuickCheck library
--
-----------------------------------------------------------------------------

module Ideas.Utils.QuickCheck
   ( module Test.QuickCheck
     -- * Data type
   , ArbGen, generator, generators
     -- * Constructors
   , arbGen, constGen, constGens, unaryGen, unaryGens
   , unaryArbGen, binaryGen, binaryGens, toArbGen
     -- * Frequency combinators
   , common, uncommon, rare, changeFrequency
   ) where

import Control.Arrow
import Control.Monad
import Data.Ratio
import Data.Semigroup as Sem
import Test.QuickCheck

---------------------------------------------------------
-- @ArbGen@ datatype

newtype ArbGen a = AG [(Rational, (Int, Gen ([a] -> a)))]

instance Sem.Semigroup (ArbGen a) where
   AG xs <> AG ys = AG (xs <> ys)

instance Monoid (ArbGen a) where
   mempty  = AG mempty
   mappend = (<>)

generator :: ArbGen a -> Gen a
generator (AG pairs) = sized rec
 where
   factor = foldr (lcm . denominator . fst) 1 pairs
   rec n  = frequency (map make (select pairs))
    where
      select
         | n == 0    = filter ((==0) . fst . snd)
         | otherwise = id
      make (r, (a, gf)) =
         let m  = round (fromInteger factor*r)
             xs = replicateM a $ rec $ n `div` 2
         in (m, gf <*> xs)

generators :: [ArbGen a] -> Gen a
generators = generator . mconcat

---------------------------------------------------------
-- Constructors

arbGen :: Arbitrary b => (b -> a) -> ArbGen a
arbGen f = newGen 0 (const . f <$> arbitrary)

constGen :: a -> ArbGen a
constGen = pureGen 0 . const

constGens :: [a] -> ArbGen a
constGens = mconcat . map constGen

unaryGen :: (a -> a) -> ArbGen a
unaryGen f = pureGen 1 (f . head)

unaryArbGen :: Arbitrary b => (b -> a -> a) -> ArbGen a
unaryArbGen f = newGen 1 $ (\a -> f a . head) <$> arbitrary

unaryGens :: [a -> a] -> ArbGen a
unaryGens = mconcat . map unaryGen

binaryGen :: (a -> a -> a) -> ArbGen a
binaryGen f = pureGen 2 (\xs -> f (head xs) (xs !! 1))

binaryGens :: [a -> a -> a] -> ArbGen a
binaryGens = mconcat . map binaryGen

pureGen :: Int -> ([a] -> a) -> ArbGen a
pureGen n = newGen n . return

toArbGen :: Gen a -> ArbGen a
toArbGen = newGen 0 . fmap const

newGen :: Int -> Gen ([a] -> a) -> ArbGen a
newGen n f = AG [(1, (n, f))]

---------------------------------------------------------
-- Frequency combinators

common, uncommon, rare :: ArbGen a -> ArbGen a
common   = changeFrequency 2
uncommon = changeFrequency (1/2)
rare     = changeFrequency (1/5)

changeFrequency :: Rational -> ArbGen a -> ArbGen a
changeFrequency r (AG xs) = AG (map (first (*r)) xs)