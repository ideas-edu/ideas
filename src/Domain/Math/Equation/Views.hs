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
module Domain.Math.Equation.Views 
   ( solvedRelations, solvedRelation
   , equationSolvedForm, solvedEquation, solvedEquations 
   , solvedRelationWith
   ) where

import Domain.Math.Expr
import Domain.Math.Data.OrList
import Domain.Math.Data.Relation
import Common.View
import Common.Rewriting
import Common.Classes

-- generalized to relation
solvedRelations :: (Crush f, Relational g) => f (g Expr) -> Bool
solvedRelations = all solvedRelation . crush

-- The variable may appear on one of the sides of the relation (right-hand side
-- is thus allowed), but must be isolated
solvedRelation :: Relational f => f Expr -> Bool
solvedRelation r =
   case (getVariable (leftHandSide r), getVariable (rightHandSide r)) of
      (Just _, Just _)  -> False
      (Just x, Nothing) -> withoutVar x (rightHandSide r)
      (Nothing, Just x) -> withoutVar x (leftHandSide r)
      _ -> hasNoVar (leftHandSide r) && hasNoVar (rightHandSide r)

-- The variable must appear on the left
solvedRelationWith :: Relational f => (Expr -> Bool) -> f Expr -> Bool
solvedRelationWith p r =
   isVariable (leftHandSide r) && p (rightHandSide r)

-------------------------------------------------------------
-- Views on equations

solvedEquations :: OrList (Equation Expr) -> Bool
solvedEquations = all solvedEquation . crush

solvedEquation :: Equation Expr -> Bool
solvedEquation eq@(lhs :==: rhs) = 
   (eq `belongsTo` equationSolvedForm) || (hasNoVar lhs && hasNoVar rhs)

equationSolvedForm :: View (Equation Expr) (String, Expr)
equationSolvedForm = makeView f g
 where
   f (Var x :==: e) | withoutVar x e =
      return (x, e)
   f _ = Nothing
   g (s, e) = Var s :==: e