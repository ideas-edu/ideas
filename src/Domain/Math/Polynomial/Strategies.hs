module Domain.Math.Polynomial.Strategies 
   ( linearStrategy, quadraticStrategy, higherDegreeStrategy
   ) where

import Prelude hiding (repeat)
import Common.Strategy
import Common.Transformation
import Domain.Math.Equation.CoverUpRules hiding (coverUpPlus)
import Domain.Math.Polynomial.Rules
import Domain.Math.Polynomial.Views
import Domain.Math.Data.OrList
import Domain.Math.Data.Equation
import Domain.Math.Expr
import Common.Uniplate
import Domain.Math.View.Basic
import Domain.Math.View.Power
import Domain.Math.SquareRoot.Views
import Domain.Math.Simplification (smartConstructors)

------------------------------------------------------------
-- Linear equations

linearStrategy :: LabeledStrategy (Equation Expr)
linearStrategy = cleanUpStrategy cleanUpSimple $
   label "Linear Equation" 
    $  label "Phase 1" (repeat (removeDivision <|> ruleOnce distribute <|> ruleMulti merge))
   <*> label "Phase 2" (try varToLeft <*> try coverUpPlus <*> try (coverUpTimes |> try coverUpNegate))

------------------------------------------------------------
-- Quadratic equations

quadraticStrategy :: LabeledStrategy (OrList (Equation Expr))
quadraticStrategy = cleanUpStrategy cleanUp $ 
   label "Quadratic Equation Strategy" $ 
   repeat $ 
         -- general form
      (  label "general form" $ 
         ( ruleOnce noConFormula <|> ruleOnce noLinFormula{- or coverup -}
           <|> ruleOnce niceFactors <|> ruleOnce simplerA )
         |> abcFormula
      )
      |> -- zero form
      (  label "zero form" $ 
         mulZero
      )
      |> -- constant form
      (  label "constant form" $ 
         coverUpPower <|> ruleOnce coverUpTimes <|> ruleOnce coverUpPlus
         <|> ruleOnce coverUpNegate <|> ruleOnce coverUpNumerator
      )
      |> -- top form
      (  label "top form" $ 
         ( ruleOnce2 (ruleSomewhere merge) <|> ruleOnce cancelTerms  
           <|> ruleOnce2 (ruleSomewhere distributionSquare)
           <|> ruleOnce2 distribute <|> ruleOnce flipEquation )
         |> ruleOnce moveToLeft
      )

-----------------------------------------------------------
-- Higher degree equations

higherDegreeStrategy :: LabeledStrategy (OrList (Equation Expr))
higherDegreeStrategy = cleanUpStrategy cleanUp $
   label "higher degree" $ 
      repeat (allPowerFactors |> (mulZero <|> ruleOnce2 powerFactor <|> sameFactor))
      <*> check isQ <*> quadraticStrategy
 
isQ :: OrList (Equation Expr) -> Bool
isQ xs = if (`belongsTo` quadraticEquationsView) xs then True else error $ show xs
 
------------------------------------------------------------
-- Cleaning up

cleanUpSimple :: Equation Expr -> Equation Expr
cleanUpSimple = fmap (smartConstructors . transform (simplify rationalView))

cleanUp :: OrList (Equation Expr) -> OrList (Equation Expr)
cleanUp (OrList xs) = OrList (filter keepEquation (map (fmap cleanUpExpr) xs))

keepEquation :: Equation Expr -> Bool
keepEquation (a :==: b) = Prelude.not (trivial || any falsity (universe a ++ universe b))  
 where
   trivial = noVars a && noVars b
   falsity (Sqrt e)  = maybe False (<0)  (match rationalView e)
   falsity (_ :/: e) = maybe False (==0) (match rationalView e)
   falsity _         = False

cleanUpExpr :: Expr -> Expr
cleanUpExpr = smartConstructors . f2 . f1 . smartConstructors . simplifyWith sumList sumView
 where
   f1 = transform (simplify powerFactorView)
   f2 = transform (simplify squareRootView)
   
   sumList = rec . map (\a -> maybe (Left a) Right $ match rationalView a) 
    where
      rec (Right r1:Right r2:rest) = rec (Right (r1+r2):rest)
      rec (Right r:xs)   = fromRational r:rec xs
      rec (Left a:xs) = a:rec xs
      rec [] = []