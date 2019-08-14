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
-- Formal mathematical properties (FMP)
--
-----------------------------------------------------------------------------

module Ideas.Text.OpenMath.FMP where

import Data.List (union)
import Ideas.Text.OpenMath.Dictionary.Quant1 (forallSymbol, existsSymbol)
import Ideas.Text.OpenMath.Dictionary.Relation1 (eqSymbol, neqSymbol)
import Ideas.Text.OpenMath.Object
import Ideas.Text.OpenMath.Symbol

data FMP = FMP
   { quantor       :: Symbol
   , metaVariables :: [String]
   , leftHandSide  :: OMOBJ
   , relation      :: Symbol
   , rightHandSide :: OMOBJ
   }

toObject :: FMP -> OMOBJ
toObject fmp
   | null (metaVariables fmp) = body
   | otherwise =
        OMBIND (OMS (quantor fmp)) (metaVariables fmp) body
 where
   body = OMA [OMS (relation fmp), leftHandSide fmp, rightHandSide fmp]

eqFMP :: OMOBJ -> OMOBJ -> FMP
eqFMP lhs rhs = FMP
   { quantor       = forallSymbol
   , metaVariables = getOMVs lhs `union` getOMVs rhs
   , leftHandSide  = lhs
   , relation      = eqSymbol
   , rightHandSide = rhs
   }

-- | Represents a common misconception. In certain (most) situations,
-- the two objects are not the same.
buggyFMP :: OMOBJ -> OMOBJ -> FMP
buggyFMP lhs rhs = (eqFMP lhs rhs)
   { quantor  = existsSymbol
   , relation = neqSymbol
   }