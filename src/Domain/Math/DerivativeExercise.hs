{-# OPTIONS -fno-case-merge #-}
-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Domain.Math.DerivativeExercise where

import Common.Uniplate (universe)
import Prelude hiding (repeat, (^))
import Domain.Math.DerivativeRules 
import Common.Strategy (Strategy, somewhere, (<*>), alternatives, label, LabeledStrategy, try)
import qualified Common.Strategy
import Common.Context (Context, liftToContext)
import Common.Exercise
import Common.Transformation
import Domain.Math.Simplification
import Domain.Math.Expr
import Domain.Math.Expr.Symbols
import Domain.Math.Expr.Parser

derivativeExercise :: Exercise Expr
derivativeExercise = makeExercise
   { description  = "Derivative"
   , exerciseCode = makeCode "math" "derivative"
   , status       = Experimental
   , parser       = parseExpr
   , equivalence  = \_ _ -> True
   , isReady      = noDiff
   , extraRules   = map liftToContext derivativeRules ++ [tidyup]
   , strategy     = derivativeStrategy
   , examples     = [ex1, ex2, ex3, ex4]
   }
   
noDiff :: Expr -> Bool
noDiff e = null [ () | Sym s _ <- universe e, s == diffSymbol ]   

derivativeStrategy :: LabeledStrategy (Context Expr)
derivativeStrategy = -- cleanUpStrategy simplify $ 
   label "Derivative" $
   try tidyup <*> Common.Strategy.repeat (derivative <*> try tidyup)

tidyup :: Rule (Context Expr)
tidyup = liftToContext $ makeSimpleRule "Tidy-up rule" $ \old -> 
   let new = simplify old
   in if old==new then Nothing else Just new
   
derivative :: Strategy (Context Expr)
derivative = somewhere $ alternatives (map liftToContext derivativeRules)

ex1, ex2, ex3 :: Expr
ex1 = diff $ lambda (Var "x") $ Var "x" ^ 2
ex2 = diff $ lambda (Var "x") $ ((1/3) :*: (x ^ fromInteger 3)) :+: (fromInteger (-3) :*: (x ^ fromInteger 2)) :+: x :+: fromInteger (-5)
 where x = Var "x"
ex3 = diff $ lambda (Var "x") (2 * Var "x") 
ex4 = diff $ lambda (Var "x") (ln (Var "x"))

main :: IO ()
main = printDerivations derivativeExercise [ex1, ex2, ex3, ex4]