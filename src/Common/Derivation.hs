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
-- Datatype for representing derivations as a tree. The datatype stores all 
-- intermediate results as well as annotations for the steps.
--
-----------------------------------------------------------------------------
module Common.Derivation
   ( -- * Data types 
     Derivation
     -- * Constructors
   , emptyDerivation
     -- * Query a derivation
   , isEmpty, derivationLength, terms, steps, triples
   , filterDerivation, derivationM, extend, prepend, lastTerm, withoutLast
   , updateSteps
   ) where

import Common.Classes

-----------------------------------------------------------------------------
-- Data type definitions for derivation trees and derivation lists

data Derivation s a = D a [(s, a)]

instance (Show s, Show a) => Show (Derivation s a) where
   show (D a xs) = unlines $
      show a : concatMap (\(r, b) -> ["   => " ++ show r, show b]) xs

instance Functor (Derivation s) where
   fmap = mapSecond

instance BiFunctor Derivation where
   biMap f g (D a xs) = D (g a) (map (biMap f g) xs)

-----------------------------------------------------------------------------
-- Inspecting a derivation

emptyDerivation :: a -> Derivation s a
emptyDerivation a = D a []

-- | Tests whether the derivation is empty
isEmpty :: Derivation s a -> Bool
isEmpty (D _ xs) = null xs

-- | Returns the number of steps in a derivation
derivationLength :: Derivation s a -> Int
derivationLength (D _ xs) = length xs

-- | All terms in a derivation
terms :: Derivation s a -> [a]
terms (D a xs) = a:map snd xs

lastTerm :: Derivation s a -> a
lastTerm = last . terms

withoutLast :: Derivation s a -> Derivation s a
withoutLast (D a xs) = D a (drop 1 xs)

-- | All steps in a derivation
steps :: Derivation s a -> [s]
steps (D _ xs) = map fst xs

-- | The triples of a derivation, consisting of the before term, the 
-- step, and the after term.
triples :: Derivation s a -> [(a, s, a)]
triples d = zip3 (terms d) (steps d) (tail (terms d))

-- | Filter steps from a derivation
filterDerivation :: (s -> a -> Bool) -> Derivation s a -> Derivation s a
filterDerivation p (D a xs) = D a (filter (uncurry p) xs)

-- | Apply a monadic function to each term, and to each step
derivationM :: Monad m => (s -> m ()) -> (a -> m ()) -> Derivation s a -> m ()
derivationM f g (D a xs) = g a >> mapM_ (\(s, b) -> f s >> g b) xs

prepend :: (a, s) -> Derivation s a -> Derivation s a
prepend (a, s) (D b xs) = D a ((s, b):xs)

extend :: Derivation s a -> (s, a) -> Derivation s a
extend (D a xs) p = D a (xs++[p])

updateSteps :: (a -> s -> a -> t) -> Derivation s a -> Derivation t a
updateSteps f d@(D x xs) = 
   let ts = [ f a b c | (a, b, c) <- triples d ]
   in D x (zip ts (map snd xs))