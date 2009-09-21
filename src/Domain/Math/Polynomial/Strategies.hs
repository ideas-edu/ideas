module Domain.Math.Polynomial.Strategies 
   ( linearStrategy, quadraticStrategy
   , higherDegreeStrategy 
   ) where

import Prelude hiding (repeat, replicate)
import Common.Strategy
import Common.Transformation
import Common.View
import Domain.Math.Equation.CoverUpRules hiding (coverUpPlus)
import Domain.Math.Polynomial.Rules
import Domain.Math.Polynomial.Views
import Domain.Math.Data.OrList
import Domain.Math.Data.Equation
import Domain.Math.Expr
import Domain.Math.Polynomial.CleanUp

------------------------------------------------------------
-- Linear equations

linearStrategy :: LabeledStrategy (Equation Expr)
linearStrategy = cleanUpStrategy (fmap cleanUpSimple) $
   label "Linear Equation" 
    $  label "Phase 1" (repeat (removeDivision <|> ruleOnce distribute <|> ruleMulti merge))
   <*> label "Phase 2" (try varToLeft <*> try (coverUpPlus id) <*> try (coverUpTimes |> try coverUpNegate))

-- helper strategy
coverUpPlus :: (Rule (Equation Expr) -> Rule a) -> Strategy a
coverUpPlus f = alternatives $ map (f . ($ oneVar))
   [coverUpPlusWith, coverUpMinusLeftWith, coverUpMinusRightWith]

------------------------------------------------------------
-- Quadratic equations

quadraticStrategy :: LabeledStrategy (OrList (Equation Expr))
quadraticStrategy = cleanUpStrategy cleanUp $ 
   label "Quadratic Equation Strategy" $ 
   repeat $ 
         -- general form
      (  label "general form" $ 
         ( ruleOnce noConFormula <|> ruleOnce noLinFormula{- or coverup -}
           <|> ruleOnce niceFactors <|> ruleOnce simplerA 
           <|> coverUpPower) -- to deal with special case x^2=0
         |> abcFormula
      )
      |> -- zero form
      (  label "zero form"
         mulZero
      )
      |> -- constant form
      (  label "constant form" $ 
         coverUpPower <|> ruleOnce coverUpTimes <|> coverUpPlus ruleOnce
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
isQ = (`belongsTo` quadraticEquationsView)