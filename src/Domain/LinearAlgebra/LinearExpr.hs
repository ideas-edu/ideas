-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (...add description...)
--
-----------------------------------------------------------------------------
module Domain.LinearAlgebra.LinearExpr 
   ( IsLinear(..), var, isVar, isConstant, renameVariables
   , splitLinearExpr, evalLinearExpr
   ) where

import Data.List
import Common.Uniplate
import Data.Maybe 
import Common.Uniplate
import GHC.Real
import Domain.Math.SExpr
import Domain.Math.Expr
import Domain.Math.Classes

class (Fractional a, Symbolic a) => IsLinear a where
   isLinear :: a -> Bool
   isVariable :: a -> Maybe String
   getVars  :: a -> [String]
   getConstant     :: a -> a
   coefficientOf   :: String -> a -> a
   
instance IsLinear Expr where

   isLinear expr = 
      case expr of
         a :+: b  -> isLinear a && isLinear b
         a :*: b  -> (isConstant a && isLinear b) || (isConstant b && isLinear a)
         a :-: b  -> isLinear a && isLinear b
         Negate a -> isLinear a
         a :/: b  -> (isConstant a && isLinear b) || (isConstant b && isLinear a)
         Var _    -> True
         _        -> isConstant expr
         
   isVariable expr =
      case expr of 
         Var s -> Just s
         _     -> Nothing
   
   getVars = collectVars
   
   getConstant expr = 
      case expr of
         a :+: b  -> getConstant a + getConstant b
         a :-: b  -> getConstant a - getConstant b
         Negate a -> negate (getConstant a)
         a :*: b  -> getConstant a * getConstant b
         a :/: b  -> getConstant a / b
         _ | isConstant expr -> expr
           | otherwise       -> 0

   coefficientOf s expr = 
      case expr of
         a :+: b  -> coefficientOf s a + coefficientOf s b
         a :-: b  -> coefficientOf s a - coefficientOf s b
         Negate a -> negate (coefficientOf s a)
         a :*: b | isConstant a -> a * coefficientOf s b
                 | isConstant b -> b * coefficientOf s a
         a :/: b | isConstant b -> coefficientOf s a / b
         Var t | s==t -> 1
         _            -> 0

instance Simplification a => IsLinear (SExprF a) where
   isLinear = isLinear . toExpr
   isVariable = isVariable . toExpr
   getVars    = getVars . toExpr
   getConstant = simplifyExpr . getConstant . toExpr
   coefficientOf s = simplifyExpr . coefficientOf s . toExpr 

splitLinearExpr :: IsLinear a => (String -> Bool) -> a -> (a, a)
splitLinearExpr f a = (make (getConstant a) xs, make 0 ys)
 where
   (xs, ys) = partition f (getVars a)
   make = foldr (\v r -> coefficientOf v a * var v + r)

evalLinearExpr :: (IsLinear a, Uniplate a) => (String -> a) -> a -> a
evalLinearExpr f a =
   case isVariable a of
      Just s  -> f s
      Nothing -> g $ map (evalLinearExpr f) cs
 where
   (cs, g) = uniplate a

renameVariables :: (IsLinear a, Uniplate a) => (String -> String) -> a -> a
renameVariables f a = 
   case isVariable a of
      Just s  -> variable (f s)
      Nothing -> g $ map (renameVariables f) cs
 where
   (cs, g) = uniplate a

isConstant :: IsLinear a => a -> Bool
isConstant = null . getVars

var :: IsLinear a => String -> a
var = variable

isVar :: IsLinear a => a -> Maybe String
isVar = isVariable