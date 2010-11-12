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
module Domain.Math.Expr.Symbolic 
   ( isBinary, WithFunctions, WithVars, Symbolic, binary, isUnary
   , isAssoBinary, fromTermWith, unary, isSymbol
   , function, getFunction, getVariable, variable, symbol
   , isVariable) where

import Common.Id
import Common.Rewriting.Term
import qualified Text.OpenMath.Symbol as OM

instance IsId OM.Symbol where
   newId s = OM.dictionary s # OM.symbolName s

instance IsSymbol OM.Symbol where
   toSymbol = toSymbol . newId

-------------------------------------------------------------------
-- Type class for symbolic representations

class (WithFunctions a, WithVars a) => Symbolic a

instance Symbolic Term

-- left-associative by default
isAssoBinary :: (IsSymbol s, Symbolic a, Monad m) => s -> a -> m (a, a)
isAssoBinary s a =
   case isFunction s a of
      Just [x, y] -> return (x, y)
      Just (x:xs) | length xs > 1 -> return (x, function s xs)
      _ -> fail "isAssoBinary"