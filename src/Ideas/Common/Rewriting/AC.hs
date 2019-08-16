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
-----------------------------------------------------------------------------

module Ideas.Common.Rewriting.AC
   ( -- * Types
     Pairings, PairingsList, PairingsPair
--   , pairings, pairingsMatch
     -- * Primitive pairings functions
   , pairingsNone, pairingsA, pairingsMatchA
   , pairingsC, pairingsAC
   ) where

import Data.List
import Ideas.Common.Classes

type Pairings     a   = a -> a -> [[(a, a)]]
type PairingsList a b = [a] -> [b] -> [[([a], [b])]]
type PairingsPair a b = (a, a) -> (b, b) -> [[(a, b)]]

-----------------------------------------------------------
-- Pairing terms with an AC theory
-- matchMode: the left-hand sides cannot have the operator at top-level

{-
pairings, pairingsMatch :: IsMagma m => m a -> Pairings a
pairings      = pairingsMode False
pairingsMatch = pairingsMode True

pairingsMode :: IsMagma m => Bool -> m a -> Pairings a
pairingsMode matchMode op =
   case (isAssociative op, isCommutative op) of
      (True , True ) -> operatorPairings op (pairingsAC matchMode)
      (True , False) -> operatorPairings op (pairingsA matchMode)
      (False, True ) -> opPairings op pairingsC
      (False, False) -> opPairings op pairingsNone
-}

-- non-associative, non-commutative pairings
pairingsNone :: PairingsPair a b
pairingsNone (a1, a2) (b1, b2) =
   [[(a1, b1), (a2, b2)]]

-- commutative pairings
pairingsC :: PairingsPair a b
pairingsC (a1, a2) (b1, b2) =
   [[(a1, b1), (a2, b2)], [(a1, b2), (a2, b1)]]

-- associative pairings
pairingsA :: Bool -> PairingsList a b
pairingsA matchMode
   | matchMode = pairingsMatchA (\a bs -> ([a], bs))
   | otherwise = rec
 where
   rec [] [] = [[]]
   rec as bs =
      [ (as1, bs1):ps
      | i <- [1 .. length as]
      , j <- [1 .. length bs]
      , i==1 || j==1
      , let (as1, as2) = splitAt i as
      , let (bs1, bs2) = splitAt j bs
      , ps <- rec as2 bs2
      ]

pairingsMatchA :: (a -> [b] -> c) -> [a] -> [b] -> [[c]]
pairingsMatchA f = rec
 where
   rec [] [] = [[]]
   rec [] _  = []
   rec (a:as) bs =
      [ p:ps
      | (xs, ys) <- take (length bs - length as) $ tail $ splits bs
      , let p = f a xs
      , ps <- rec as ys
      ]

-- go _ = length $ pairingsMatchA [1::Int ..10] [1::Int ..20] -- 92378

-- associative/commutative pairings
pairingsAC :: Bool -> PairingsList a b
pairingsAC matchMode = rec
 where
   rec [] [] = [[]]
   rec [] _  = []
   rec (a:as) bs =
      [ (as1, bs1):ps
      | (asr, as2) <- if matchMode then [([], as)] else divide as
      , let as1 = a:asr
      , (bs1, bs2) <- divide bs
      , not (null bs1)
      , length as1==1 || length bs1==1
      , ps <- rec as2 bs2
      ]

----------------------------------------------------------
-- Helper functions
{-
opPairings :: IsMagma m => m a -> PairingsPair a a -> Pairings a
opPairings op f a b = fromMaybe [] $
   liftM2 f (match (magmaView op) a) (match (magmaView op) b)

operatorPairings :: IsMagma m => m a -> PairingsList a a -> Pairings a
operatorPairings op g = curry $
   let f a = fromMaybe [a] $ match (magmaListView op) a
       h = build (magmaListView op)
   in map (map (onBoth h)) . uncurry g . onBoth f
-}
divide :: [a] -> [([a], [a])]
divide = foldr op [([], [])]
 where
   op a ps = map (mapFirst (a:)) ps ++ map (mapSecond (a:)) ps

splits :: [a] -> [([a], [a])]
splits xs = zip (inits xs) (tails xs)

{-
onBoth :: (a -> b) -> (a, a) -> (b, b)
onBoth f (x, y) = (f x, f y)

permutations :: [a] -> [[a]]
permutations = foldr (concatMap . insert) [[]]
 where
   insert a []     = [[a]]
   insert a (x:xs) = (a:x:xs) : map (x:) (insert a xs)
-}