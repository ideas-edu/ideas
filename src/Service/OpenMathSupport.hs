{-# LANGUAGE Rank2Types #-}
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
module Service.OpenMathSupport
   ( -- * Conversion functions to/from OpenMath
     toOpenMath, fromOpenMath, noMixedFractions
   , toOMOBJ, fromOMOBJ
   ) where

import Common.Library
import Common.Utils.Uniplate
import Control.Monad
import Data.Char
import Data.List
import Text.OpenMath.Dictionary.Arith1
import Text.OpenMath.Dictionary.Fns1
import Text.OpenMath.Object
import qualified Text.OpenMath.Symbol as OM

-----------------------------------------------------------------------------
-- Utility functions for conversion to/from OpenMath

toOpenMath :: Monad m => Exercise a -> a -> m OMOBJ
toOpenMath ex a = do
   v <- hasTermViewM ex
   return (toOMOBJ (build v a))

fromOpenMath :: MonadPlus m => Exercise a -> OMOBJ -> m a
fromOpenMath ex omobj = do
   v <- hasTermViewM ex
   a <- fromOMOBJ omobj
   matchM v a

toOMOBJ :: IsTerm a => a -> OMOBJ
toOMOBJ = rec . toTerm
 where
   rec term =
      case term of
         TVar s    -> OMV s
         TCon s    -> OMS (idToSymbol (getId s))
         TMeta i   -> OMV ('$' : show i)
         TNum n    -> OMI n
         TFloat d  -> OMF d
         TApp _ _  -> let (f, xs) = getSpine term
                      in make (map rec (f:xs))

   make [OMS s, OMV x, body] | s == lambdaSymbol =
      OMBIND (OMS s) [x] body
   make xs = OMA xs

fromOMOBJ :: (MonadPlus m, IsTerm a) => OMOBJ -> m a
fromOMOBJ = (>>= fromTerm) . rec
 where
   rec omobj =
      case omobj of
         OMV x -> case isMeta x of
                     Just n  -> return (TMeta n)
                     Nothing -> return (TVar x)
         OMS s -> return (symbol (newSymbol (OM.dictionary s # OM.symbolName s)))
         OMI n -> return (TNum n)
         OMF a -> return (TFloat a)
         OMA (x:xs) -> liftM2 makeTerm (rec x) (mapM rec xs)
         OMBIND binder xs body ->
            rec (OMA (binder:map OMV xs++[body]))
         _ -> fail "Invalid OpenMath object"

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

hasTermViewM  :: Monad m => Exercise a -> m (View Term a)
hasTermViewM = maybe (fail "No support for terms") return . hasTermView

mfSymbol :: OM.Symbol
mfSymbol = OM.makeSymbol "extra" "mixedfraction"