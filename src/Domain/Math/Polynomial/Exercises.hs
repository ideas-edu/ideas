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
module Domain.Math.Polynomial.Exercises 
   ( linearExercise, linearMixedExercise
   , quadraticExercise, quadraticNoABCExercise
   , quadraticWithApproximation
   , higherDegreeExercise, findFactorsExercise
   ) where

import Common.Library
import Control.Monad
import Data.Maybe
import Domain.Math.Approximation
import Domain.Math.Data.OrList
import Domain.Math.Data.Relation
import Domain.Math.Equation.CoverUpRules
import Domain.Math.Equation.Views
import Domain.Math.Polynomial.Examples
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import Domain.Math.Polynomial.BuggyRules
import Domain.Math.CleanUp
import Domain.Math.Polynomial.Equivalence
import Domain.Math.Polynomial.Rules
import Domain.Math.Polynomial.Strategies
import Domain.Math.Polynomial.Views
import qualified Data.Traversable as T
import qualified Data.Foldable as F

------------------------------------------------------------
-- Exercises

linearExercise :: Exercise (Equation Expr)
linearExercise = makeExercise 
   { exerciseId   = describe "solve a linear equation" $ 
                       newId "algebra.equations.linear"
   , status       = Provisional
   , parser       = parseExprWith (pEquation pExpr)
   , similarity   = viewEquivalent (traverseView cleanUpACView)
   , equivalence  = viewEquivalent linearEquationView
   , isSuitable   = (`belongsTo` linearEquationView)
   , isReady      = solvedRelationWith $ \a -> 
                       a `belongsTo` mixedFractionNormalForm || 
                       a `belongsTo` rationalNormalForm
   , extraRules   = map use buggyRulesEquation ++
                    map use buggyRulesExpr 
   , ruleOrdering = ruleOrderingWithId
                       [ getId coverUpTimes, getId flipEquation
                       , getId removeDivision
                       ]
   , strategy     = linearStrategy
   , navigation   = termNavigator
   , examples     = linearExamples
   }
      
linearMixedExercise :: Exercise (Equation Expr)
linearMixedExercise = linearExercise 
   { exerciseId   = describe "solve a linear equation with mixed fractions" $ 
                       newId "algebra.equations.linear.mixed"
   , isReady      = solvedRelationWith (`belongsTo` mixedFractionNormalForm)
   , strategy     = linearMixedStrategy
   } 

quadraticExercise :: Exercise (OrList (Relation Expr))
quadraticExercise = makeExercise 
   { exerciseId   = describe "solve a quadratic equation" $ 
                       newId "algebra.equations.quadratic"
   , status       = Provisional
   , parser       = parseExprWith (pOrList (pEquation pExpr)) 
                       >>> right (build (traverseView equationView)) 
   , similarity   = viewEquivalent (traverseView (traverseView cleanUpView))
   , equivalence  = equivalentRelation (viewEquivalent quadraticEquationsView)
   , isSuitable   = (`belongsTo` (traverseView equationView >>> quadraticEquationsView))
   , isReady      = solvedRelations
   , extraRules   = map use abcBuggyRules ++ buggyQuadratic ++
                    map use buggyRulesEquation ++ map use buggyRulesExpr 
   , ruleOrdering = ruleOrderingWithId $ 
                       quadraticRuleOrder ++ [getId buggySquareMultiplication]
   , strategy     = quadraticStrategy
   , navigation   = termNavigator
   , examples     = mapExamples (singleton . build equationView) quadraticExamples
   }

higherDegreeExercise :: Exercise (OrList (Relation Expr))
higherDegreeExercise = makeExercise 
   { exerciseId    = describe "solve an equation (higher degree)" $
                        newId "algebra.equations.polynomial"
   , status        = Provisional
   , parser        = parser quadraticExercise
   , similarity    = viewEquivalent (traverseView (traverseView cleanUpView))
   , eqWithContext = Just $ eqAfterSubstitution $ 
                        equivalentRelation (viewEquivalent higherDegreeEquationsView)
   , isSuitable    = (`belongsTo` (traverseView equationView >>> higherDegreeEquationsView))
   , isReady       = solvedRelations
   , extraRules    = map use abcBuggyRules ++ buggyQuadratic ++
                     map use buggyRulesEquation ++ map use buggyRulesExpr 
   , ruleOrdering  = ruleOrderingWithId quadraticRuleOrder
   , strategy      = higherDegreeStrategy
   , navigation    = termNavigator
   , examples      = mapExamples (singleton . build equationView) higherDegreeExamples
   }
   
quadraticNoABCExercise :: Exercise (OrList (Relation Expr))
quadraticNoABCExercise = quadraticExercise
   { exerciseId   = describe "solve a quadratic equation without abc-formula" $ 
                       newId "algebra.equations.quadratic.no-abc"
   , status       = Alpha
   , strategy     = configure cfg quadraticStrategy
   }
 where
   cfg = [ (byName prepareSplitSquare, Reinsert)
         , (byName bringAToOne, Reinsert)
         , (byName (newId "abc form"), Remove)
         , (byName simplerPolynomial, Remove)
         ]
         
quadraticWithApproximation :: Exercise (OrList (Relation Expr))
quadraticWithApproximation = quadraticExercise
   { exerciseId   = describe "solve a quadratic equation with approximation" $ 
                       newId "algebra.equations.quadratic.approximate"
   , status       = Alpha
   , parser       = parseExprWith (pOrList (pRelation pExpr))
   , strategy     = configure cfg quadraticStrategy
   , equivalence  = equivalentApprox
   }
 where
   cfg = [ (byName (newId "approximate result"), Reinsert)
         , (byName (newId "square root simplification"), Remove)
         ]

findFactorsExercise :: Exercise Expr
findFactorsExercise = makeExercise
   { exerciseId   = describe "factorize the expression" $ 
                       newId "algebra.manipulation.polynomial.factor"
   , status       = Provisional
   , parser       = parseExprWith pExpr
   , similarity   = \a b -> cleanUpExpr a == cleanUpExpr b
   , equivalence  = viewEquivalent (polyViewWith rationalView)
   , isReady      = (`belongsTo` linearFactorsView)
   , strategy     = findFactorsStrategy
   , navigation   = termNavigator
   , extraRules   = map liftToContext buggyRulesExpr
   , examples     = factorizeExamples
   }

linearFactorsView :: View Expr (Bool, [(String, Expr, Expr)])
linearFactorsView = productView >>> second (listView myLinearView)
 where
   myLinearView :: View Expr (String, Expr, Expr)
   myLinearView = makeView f (build linearView)
   
   f expr = do 
      triple@(_, e1, e2) <- match linearView expr 
      a <- match integerView e1
      b <- match integerView e2
      guard (a > 0 && gcd a b == 1) -- gcd 0 0 is undefined
      return triple
    `mplus` do
      guard (expr `belongsTo` rationalView)
      return ("x", 0, expr)

--------------------------------------------
-- Equality

equivalentApprox :: OrList (Relation Expr) -> OrList (Relation Expr) -> Bool
equivalentApprox a b
   | hasApprox a || hasApprox b = 
        let norm = liftM ( simplify orSetView 
                         . fmap (fmap (acExpr . cleanUpExpr) . toApprox)  
                         . simplify quadraticEquationsView
                         ) . T.mapM toEq
        in fromMaybe False $ liftM2 (==) (norm a) (norm b)
   | otherwise =
        equivalentRelation (viewEquivalent quadraticEquationsView) a b 
 where
   hasApprox = F.any isApproximately
   isApproximately = (==Approximately) . relationType
   toEq rel | relationType rel `elem` [EqualTo, Approximately] = 
      Just (leftHandSide rel :==: rightHandSide rel)
            | otherwise = Nothing

toApprox :: Equation Expr -> Relation Expr
toApprox (a :==: b) = f a .~=. f b
 where
   f x = maybe x (Number . precision 4) (match doubleView x)
      
equivalentRelation :: (OrList (Equation a) -> OrList (Equation a) -> Bool) -> OrList (Relation a) -> OrList (Relation a) -> Bool
equivalentRelation f ra rb = fromMaybe False $ do
   a <- T.mapM (match equationView) ra
   b <- T.mapM (match equationView) rb
   return (f a b)