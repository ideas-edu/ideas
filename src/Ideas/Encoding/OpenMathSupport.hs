{-# LANGUAGE Rank2Types #-}
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

module Ideas.Encoding.OpenMathSupport
   ( -- * Conversion functions to/from OpenMath
     toOpenMath, fromOpenMath, noMixedFractions
   , toOMOBJ, fromOMOBJ
   ) where

import Data.Char
import Data.List
import Ideas.Common.Library
import Ideas.Text.OpenMath.Dictionary.Arith1
import Ideas.Text.OpenMath.Dictionary.Fns1
import Ideas.Text.OpenMath.Object
import Ideas.Utils.Uniplate
import qualified Ideas.Text.OpenMath.Dictionary.List1 as OM
import qualified Ideas.Text.OpenMath.Symbol as OM

-----------------------------------------------------------------------------
-- Utility functions for conversion to/from OpenMath

toOpenMath :: Exercise a -> a -> Maybe OMOBJ
toOpenMath ex a = do
   v <- hasTermView ex
   return (toOMOBJ (build v a))

fromOpenMath :: Exercise a -> OMOBJ -> Maybe a
fromOpenMath ex omobj = do
   v <- hasTermView ex
   a <- fromOMOBJ omobj
   match v a

toOMOBJ :: IsTerm a => a -> OMOBJ
toOMOBJ = rec . toTerm
 where
   rec term =
      case term of
         TVar s    -> OMV s
         TCon s xs
            | null xs   -> OMS (idToSymbol (getId s))
            | otherwise -> make (OMS (idToSymbol (getId s)):map rec xs)
         TMeta i   -> OMV ('$' : show i)
         TNum n    -> OMI n
         TFloat d  -> OMF d
         TList xs  -> rec (function (newSymbol OM.listSymbol) xs)

   make [OMS s, OMV x, body] | s == lambdaSymbol =
      OMBIND (OMS s) [x] body
   make xs = OMA xs

fromOMOBJ :: IsTerm a => OMOBJ -> Maybe a
fromOMOBJ = (>>= fromTerm) . rec
 where
   rec omobj =
      case omobj of
         OMV x  -> case isMeta x of
                      Just n  -> Just (TMeta n)
                      Nothing -> Just (TVar x)
         OMS s  -> Just (symbol (newSymbol (OM.dictionary s, OM.symbolName s)))
         OMI n  -> Just (TNum n)
         OMF a  -> Just (TFloat a)
         OMA xs -> case xs of
                      OMS s:ys | s == OM.listSymbol -> TList <$> mapM rec ys
                               | otherwise -> function (newSymbol s) <$> mapM rec ys
                      _ -> Nothing
         OMBIND binder xs body ->
            rec (OMA (binder:map OMV xs++[body]))

   isMeta ('$':xs) = Just (foldl' (\a b -> a*10+ord b-48) 0 xs) -- '
   isMeta _        = Nothing

noMixedFractions :: OMOBJ -> OMOBJ
noMixedFractions = transform f
 where
   f (OMA [OMS s, a, b, c]) | s == mfSymbol =
      OMA [OMS plusSymbol, a, OMA [OMS divideSymbol, b, c]]
   f a = a

idToSymbol :: Id -> OM.Symbol
idToSymbol a
   | null (qualifiers a) =
        OM.extraSymbol (unqualified a)
   | otherwise =
        OM.makeSymbol (qualification a) (unqualified a)

mfSymbol :: OM.Symbol
mfSymbol = OM.makeSymbol "extra" "mixedfraction"