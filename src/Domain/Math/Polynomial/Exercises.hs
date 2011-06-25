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
   , higherDegreeExercise
   , findFactorsExercise, expandExercise
   ) where

import Common.Library
import Control.Monad
import Data.Function
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
   , parser       = parseEqExpr
   , similarity   = withoutContext (viewEquivalent (traverseView cleanUpACView))
   , equivalence  = withoutContext (viewEquivalent linearEquationView)
   , suitable     = predicateView linearEquationView
   , ready        = predicateView (equationSolvedWith mixedFractionNF)
                    <||> predicateView (equationSolvedWith rationalNF)
                    <||> predicateView (equationSolvedWith doubleNF)
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
   , ready        = predicateView (equationSolvedWith mixedFractionNF)
   , strategy     = linearMixedStrategy
   } 

quadraticExercise :: Exercise (OrList (Relation Expr))
quadraticExercise = makeExercise 
   { exerciseId   = describe "solve a quadratic equation" $ 
                       newId "algebra.equations.quadratic"
   , status       = Provisional
   , parser       = parseOrsEqExpr
                       >>> right (build (traverseView equationView)) 
   , similarity   = withoutContext (viewEquivalent (traverseView (traverseView cleanUpView)))
   , equivalence  = withoutContext (equivalentRelation (viewEquivalent quadraticEquationsView))
   , suitable     = predicateView (traverseView equationView >>> quadraticEquationsView)
   , ready        = predicateView relationsSolvedForm
   , extraRules   = map use abcBuggyRules ++ buggyQuadratic ++
                    map use buggyRulesEquation ++ map use buggyRulesExpr 
   , ruleOrdering = ruleOrderingWithId $ 
                       quadraticRuleOrder ++ [getId buggySquareMultiplication]
   , splitParts   = splitOrList
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
   , similarity    = withoutContext (viewEquivalent (traverseView (traverseView cleanUpView)))
   , equivalence   = eqAfterSubstitution $ 
                        equivalentRelation (viewEquivalent higherDegreeEquationsView)
   , suitable      = predicateView (traverseView equationView >>> higherDegreeEquationsView)
   , ready         = predicateView relationsSolvedForm
   , extraRules    = map use abcBuggyRules ++ buggyQuadratic ++
                     map use buggyRulesEquation ++ map use buggyRulesExpr 
   , ruleOrdering  = ruleOrderingWithId quadraticRuleOrder
   , splitParts    = splitOrList
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
   , parser       = parseOrsRelExpr
   , strategy     = configure cfg quadraticStrategy
   , equivalence  = withoutContext equivalentApprox
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
   , parser       = parseExpr
   , similarity   = withoutContext ((==) `on` cleanUpExpr)
   , equivalence  = withoutContext (viewEquivalent (polyViewWith rationalView))
   , ready        = predicateView linearFactorsView
   , ruleOrdering = ruleOrderingWithId quadraticRuleOrder
   , strategy     = findFactorsStrategy
   , navigation   = termNavigator
   , extraRules   = map liftToContext buggyRulesExpr
   , examples     = factorizeExamples
   }

expandExercise :: Exercise Expr
expandExercise = makeExercise
   { exerciseId   = describe "expand an expression to polynomial normal form" $ 
                       newId "algebra.manipulation.polynomial.expand"
   , status       = Provisional
   , parser       = parseExpr
   , similarity   = withoutContext ((==) `on` cleanUpExpr)
   , equivalence  = withoutContext (viewEquivalent (polyViewWith rationalView))
   , ready        = predicateView (polyNormalForm rationalView)
   , ruleOrdering = ruleOrderingWithId (getId fractionProduct:quadraticRuleOrder)
   , strategy     = expandStrategy
   , navigation   = termNavigator
   , extraRules   = map liftToContext buggyRulesExpr
   , examples     = expandExamples
   }

linearFactorsView :: View Expr (Bool, [(String, Expr, Expr)])
linearFactorsView = toView productView >>> second (listView myLinearView)
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
   f x = maybe x (fromDouble . precision 4) (match doubleView x)
      
equivalentRelation :: (OrList (Equation a) -> OrList (Equation a) -> Bool) -> OrList (Relation a) -> OrList (Relation a) -> Bool
equivalentRelation f ra rb = fromMaybe False $ do
   a <- T.mapM (match equationView) ra
   b <- T.mapM (match equationView) rb
   return (f a b)
   
splitOrList :: OrList a -> [OrList a]
splitOrList p 
   | isTrue p  = [p]
   | otherwise = map singleton (F.toList p)