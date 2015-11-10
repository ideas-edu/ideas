-----------------------------------------------------------------------------
-- Copyright 2015, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Datatype for representing a derivation (parameterized both in the terms
-- and the steps)
--
-----------------------------------------------------------------------------

module Ideas.Common.Derivation
   ( -- * Data type
     Derivation
     -- * Constructing a derivation
   , emptyDerivation, prepend, extend
     -- * Querying a derivation
   , isEmpty, derivationLength, terms, steps, triples
   , firstTerm, lastTerm, lastStep, withoutLast
   , updateSteps, derivationM
   ) where

import Data.Maybe
import Ideas.Common.Classes
import qualified Data.Foldable as F
import qualified Data.Sequence as S

-----------------------------------------------------------------------------
-- Data type definition and instances

data Derivation s a = D a (S.Seq (s, a))

instance (Show s, Show a) => Show (Derivation s a) where
   show (D a xs) = unlines $
      show a : concatMap (\(r, b) -> ["   => " ++ show r, show b]) (F.toList xs)

instance Functor (Derivation s) where
   fmap = mapSecond

instance BiFunctor Derivation where
   biMap f g (D a xs) = D (g a) (fmap (biMap f g) xs)

-----------------------------------------------------------------------------
-- Constructing a derivation

emptyDerivation :: a -> Derivation s a
emptyDerivation a = D a S.empty

prepend :: (a, s) -> Derivation s a -> Derivation s a
prepend (a, s) (D b xs) = D a ((s, b) S.<| xs)

extend :: Derivation s a -> (s, a) -> Derivation s a
extend (D a xs) p = D a (xs S.|> p)

-----------------------------------------------------------------------------
-- Querying a derivation

-- | Tests whether the derivation is empty
isEmpty :: Derivation s a -> Bool
isEmpty (D _ xs) = S.null xs

-- | Returns the number of steps in a derivation
derivationLength :: Derivation s a -> Int
derivationLength (D _ xs) = S.length xs

-- | All terms in a derivation
terms :: Derivation s a -> [a]
terms (D a xs) = a:map snd (F.toList xs)

-- | All steps in a derivation
steps :: Derivation s a -> [s]
steps (D _ xs) = map fst (F.toList xs)

-- | The triples of a derivation, consisting of the before term, the
-- step, and the after term.
triples :: Derivation s a -> [(a, s, a)]
triples d = zip3 (terms d) (steps d) (tail (terms d))

firstTerm :: Derivation s a -> a
firstTerm = head . terms

lastTerm :: Derivation s a -> a
lastTerm = last . terms

lastStep:: Derivation s a -> Maybe s
lastStep = listToMaybe . reverse . steps

withoutLast :: Derivation s a -> Derivation s a
withoutLast d@(D a xs) =
   case S.viewr xs of
      S.EmptyR  -> d
      ys S.:> _ -> D a ys

updateSteps :: (a -> s -> a -> t) -> Derivation s a -> Derivation t a
updateSteps f d =
   let ts   = [ f a b c | (a, b, c) <- triples d ]
       x:xs = terms d
   in D x (S.fromList (zip ts xs))

-- | Apply a monadic function to each term, and to each step
derivationM :: Monad m => (s -> m ()) -> (a -> m ()) -> Derivation s a -> m ()
derivationM f g (D a xs) = g a >> mapM_ (\(s, b) -> f s >> g b) (F.toList xs)