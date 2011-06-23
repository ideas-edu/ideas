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
   ( relationSolvedForm, relationsSolvedForm
   , equationSolvedForm, equationsSolvedForm, equationSolvedWith
   ) where

import Domain.Math.Expr
import Domain.Math.Data.OrList
import Domain.Math.Data.Relation
import Common.Id
import Common.View
import Data.Traversable

relationsSolvedForm :: (Traversable f, Relational g) => 
   View (f (g Expr)) (f (Expr -> Expr -> g Expr, String, Expr))
relationsSolvedForm = "relations.solved" @> traverseView relationSolvedForm

-- The variable may appear on one of the sides of the relation (right-hand side
-- is thus allowed), but must be isolated
relationSolvedForm :: Relational f => 
   View (f Expr) (Expr -> Expr -> f Expr, String, Expr)
relationSolvedForm = "relation.solved" @> makeView f g
 where
   f r = case (getVariable (leftHandSide r), getVariable (rightHandSide r)) of
            (Just x, Nothing) | withoutVar x (rightHandSide r) ->
               return (constructor r, x, rightHandSide r)
            (Nothing, Just x) | withoutVar x (leftHandSide r) ->
               return (flip (constructor r), x, leftHandSide r)
            _ -> Nothing
   g (make, s, e) = make (Var s) e

-------------------------------------------------------------
-- Views on equations

equationsSolvedForm :: View (OrList (Equation Expr)) (OrList (String, Expr))
equationsSolvedForm = "equations.solved" @> traverseView equationSolvedForm

equationSolvedForm :: View (Equation Expr) (String, Expr)
equationSolvedForm = "equation.solved" @> makeView f g
 where
   f (Var x :==: e) | withoutVar x e =
      return (x, e)
   f _ = Nothing
   g (s, e) = Var s :==: e
   
equationSolvedWith :: View Expr a -> View (Equation Expr) (String, a)
equationSolvedWith v = "equation.solved-with" @> makeView f g
 where
   f (lhs :==: rhs) = do
      x <- getVariable lhs
      a <- match v rhs
      return (x, a)
   g (s, a) = Var s :==: build v a