module Domain.Math.Equation.Views (equationSolvedForm) where

import Domain.Math.Expr
import Domain.Math.Data.Equation
import Common.View

-------------------------------------------------------------
-- Views on equations

equationSolvedForm :: View (Equation Expr) (String, Expr)
equationSolvedForm = makeView f g
 where
   f (Var x :==: e) | x `notElem` collectVars e =
      return (x, e)
   f _ = Nothing
   g (s, e) = Var s :==: e