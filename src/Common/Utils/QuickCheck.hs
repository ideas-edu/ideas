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
-- Extensions to the QuickCheck library
--
-----------------------------------------------------------------------------
module Common.Utils.QuickCheck 
   ( module Test.QuickCheck
     -- * Data type
   , ArbGen, generator, generators
     -- * Constructors
   , arbGen, constGen, unaryGen, binaryGen, binaryGens
     -- * Frequency combinators
   , common, uncommon, rare, changeFrequency
   ) where

import Control.Arrow
import Control.Monad
import Data.Monoid
import Data.Ratio
import Test.QuickCheck

---------------------------------------------------------
-- @ArbGen@ datatype

newtype ArbGen a = AG [(Rational, (Int, Gen ([a] -> a)))]

instance Monoid (ArbGen a) where
   mempty = AG mempty
   AG xs `mappend` AG ys = AG (xs `mappend` ys)

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
         in (m, liftM2 ($) gf xs)

generators :: [ArbGen a] -> Gen a
generators = generator . mconcat

---------------------------------------------------------
-- Constructors

arbGen :: Arbitrary b => (b -> a) -> ArbGen a
arbGen f = newGen 0 (liftM (const . f) arbitrary)

constGen :: a -> ArbGen a
constGen = pureGen 0 . const

unaryGen :: (a -> a) -> ArbGen a
unaryGen f = pureGen 1 $ \[x] -> f x

binaryGen :: (a -> a -> a) -> ArbGen a
binaryGen f = pureGen 2 $ \[x, y] -> f x y

binaryGens :: [(a -> a -> a)] -> ArbGen a
binaryGens = mconcat . map binaryGen

pureGen :: Int -> ([a] -> a) -> ArbGen a
pureGen n = newGen n . return

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
