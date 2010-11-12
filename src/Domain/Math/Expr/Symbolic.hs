{-# LANGUAGE TypeSynonymInstances #-}
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
-----------------------------------------------------------------------------
module Domain.Math.Expr.Symbolic where

import Common.Id
import Common.Rewriting
import qualified Text.OpenMath.Symbol as OM

instance IsId OM.Symbol where
   newId s = OM.dictionary s # OM.symbolName s

instance IsSymbol OM.Symbol where
   toSymbol = toSymbol . newId