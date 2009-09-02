module Domain.Math.Equation.Views 
   ( equationSolvedForm, solvedEquation, solvedEquations ) where

import Domain.Math.Expr
import Domain.Math.Data.OrList
import Domain.Math.Data.Equation
import Common.View

-------------------------------------------------------------
-- Views on equations

solvedEquations :: OrList (Equation Expr) -> Bool
solvedEquations (OrList xs) = all solvedEquation xs

solvedEquation :: Equation Expr -> Bool
solvedEquation eq@(lhs :==: rhs) = 
   (eq `belongsTo` equationSolvedForm) || (noVars lhs && noVars rhs)

equationSolvedForm :: View (Equation Expr) (String, Expr)
equationSolvedForm = makeView f g
 where
   f (Var x :==: e) | x `notElem` collectVars e =
      return (x, e)
   f _ = Nothing
   g (s, e) = Var s :==: e