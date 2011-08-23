-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Common.Rewriting.AC
   ( -- * Types
     Pairings, PairingsList, PairingsPair
--   , pairings, pairingsMatch
     -- * Primitive pairings functions
   , pairingsNone, pairingsA
   , pairingsC, pairingsAC
   ) where

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
pairingsA matchMode = rec
 where
   rec [] [] = [[]]
   rec as bs =
      [ (a1, b1):ps
      | i <- if matchMode && not (null as) then [1] else [1 .. length as]
      , j <- [1 .. length bs]
      , i==1 || j==1
      , let (as1, as2) = splitAt i as
      , let (bs1, bs2) = splitAt j bs
      , let a1 = as1
      , let b1 = bs1
      , ps <- rec as2 bs2
      ]

-- associative/commutative pairings
pairingsAC :: Bool -> PairingsList a b
pairingsAC matchMode = rec
 where
   rec [] [] = [[]]
   rec [] _  = []
   rec (a:as) bs =
      [ (as1, bs1):ps
      | (asr, as2) <- if matchMode then [([], as)] else splits as
      , let as1 = a:asr
      , (bs1, bs2) <- splits bs
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
splits :: [a] -> [([a], [a])]
splits = foldr insert [([], [])]
 where
   insert a ps =
      let toLeft  (xs, ys) = (a:xs,   ys)
          toRight (xs, ys) = (  xs, a:ys)
      in map toLeft ps ++ map toRight ps

{-
onBoth :: (a -> b) -> (a, a) -> (b, b)
onBoth f (x, y) = (f x, f y)

permutations :: [a] -> [[a]]
permutations = foldr (concatMap . insert) [[]]
 where
   insert a []     = [[a]]
   insert a (x:xs) = (a:x:xs) : map (x:) (insert a xs)
-}