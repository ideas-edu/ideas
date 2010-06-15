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
module Domain.Math.Polynomial.Exercises where

import Domain.Math.Approximation
import Common.Context
import Common.Exercise
import Common.Rewriting
import Common.Strategy
import Common.Traversable
import Common.Transformation
import Common.View
import Data.Maybe
import Domain.Math.Data.OrList
import Domain.Math.Data.Relation
import Domain.Math.Equation.Views
import Domain.Math.Examples.DWO1
import Domain.Math.Examples.DWO2
import Domain.Math.Expr
import Domain.Math.Polynomial.BuggyRules
import Domain.Math.Polynomial.CleanUp
import Domain.Math.Polynomial.Rules
import Domain.Math.Polynomial.Strategies
import Domain.Math.Polynomial.Views
import Domain.Math.Polynomial.Equivalence
import Domain.Math.Numeric.Views
import Domain.Math.Equation.CoverUpRules
import Control.Monad

------------------------------------------------------------
-- Exercises

linearExercise :: Exercise (Equation Expr)
linearExercise = makeExercise 
   { description  = "solve a linear equation"
   , exerciseCode = makeCode "math" "lineq"
   , status       = Provisional
   , parser       = parseExprWith (pEquation pExpr)
   , similarity   = eqRelation (acExpr . cleanUpExpr)
   , equivalence  = viewEquivalent linearEquationView
   , isSuitable   = (`belongsTo` linearEquationView)
   , isReady      = solvedRelationWith $ \a -> 
                       a `belongsTo` mixedFractionNormalForm || 
                       a `belongsTo` rationalNormalForm
   , extraRules   = map use buggyRulesEquation ++
                    map use buggyRulesExpr 
   , ruleOrdering = ruleNameOrderingWith 
                       [ name coverUpTimes, name flipEquation
                       , name removeDivision
                       ]
   , strategy     = linearStrategy
   , navigation   = exprNavigator
   , examples     = concat (linearEquations ++ [specialCases])
   }
 where
   specialCases = 
      let x = Var "x" 
      in [5 :==: x, 5 :==: x + 1, x - 1/5 :==: 2]
      
linearMixedExercise :: Exercise (Equation Expr)
linearMixedExercise = linearExercise 
   { description  = "solve a linear equation with mixed fractions"
   , exerciseCode = makeCode "math" "lineq-mixed"
   , isReady      = solvedRelationWith (`belongsTo` mixedFractionNormalForm)
   , strategy     = linearMixedStrategy
   } 

quadraticExercise :: Exercise (OrList (Relation Expr))
quadraticExercise = makeExercise 
   { description  = "solve a quadratic equation"
   , exerciseCode = makeCode "math" "quadreq"
   , status       = Provisional
   , parser       = \input -> case parseExprWith (pOrList (pEquation pExpr)) input of
                                 Left err -> Left err
                                 Right xs -> Right (build (switchView equationView) xs)
   , similarity   = eqOrList cleanUpExpr2
   , equivalence  = equivalentRelation (viewEquivalent quadraticEquationsView)
   , isSuitable   = (`belongsTo` (switchView equationView >>> quadraticEquationsView))
   , isReady      = solvedRelations
   , extraRules   = map use abcBuggyRules ++ buggyQuadratic ++
                    map use buggyRulesEquation ++ map use buggyRulesExpr 
   , ruleOrdering = ruleNameOrderingWith $ 
                       quadraticRuleOrder ++ [name buggySquareMultiplication]
   , strategy     = quadraticStrategy
   , navigation   = exprNavigator
   , examples     = map (orList . return . build equationView) (concat quadraticEquations)
   }
   
higherDegreeExercise :: Exercise (OrList (Relation Expr))
higherDegreeExercise = makeExercise 
   { description   = "solve an equation (higher degree)"
   , exerciseCode  = makeCode "math" "higherdegree"
   , status        = Provisional
   , parser        = parser quadraticExercise
   , similarity    = eqOrList cleanUpExpr2
   , eqWithContext = Just $ eqAfterSubstitution $ 
                        equivalentRelation (viewEquivalent higherDegreeEquationsView)
   , isSuitable    = (`belongsTo` (switchView equationView >>> higherDegreeEquationsView))
   , isReady       = solvedRelations
   , extraRules    = map use abcBuggyRules ++ buggyQuadratic ++
                     map use buggyRulesEquation ++ map use buggyRulesExpr 
   , ruleOrdering = ruleNameOrderingWith quadraticRuleOrder
   , strategy      = higherDegreeStrategy
   , navigation   = exprNavigator
   , examples      = map (orList . return . build equationView) 
                        (concat $ higherEq1 ++ higherEq2 ++ [higherDegreeEquations])
   }
   
quadraticNoABCExercise :: Exercise (OrList (Relation Expr))
quadraticNoABCExercise = quadraticExercise
   { description  = "solve a quadratic equation without abc-formula"
   , exerciseCode = makeCode "math" "quadreq-no-abc"
   , status       = Alpha
   , strategy     = configure cfg quadraticStrategy
   }
 where
   cfg = [ (ByName (name prepareSplitSquare), Reinsert)
         , (ByName (name bringAToOne), Reinsert)
         , (ByName "abc form", Remove)
         , (ByName (name simplerPolynomial), Remove)
         ]
         
quadraticWithApproximation :: Exercise (OrList (Relation Expr))
quadraticWithApproximation = quadraticExercise
   { description  = "solve a quadratic equation with approximation"
   , exerciseCode = makeCode "math" "quadreq-with-approx"
   , status       = Alpha
   , parser       = parseExprWith (pOrList (pRelation pExpr))
   , strategy     = configure cfg quadraticStrategy
   , equivalence  = equivalentApprox
   }
 where
   cfg = [ (ByName "approximate result", Reinsert)
         , (ByName "square root simplification", Remove)
         ]

findFactorsExercise :: Exercise Expr
findFactorsExercise = makeExercise
   { description  = "factorize the expression"
   , exerciseCode = makeCode "math" "factor"
   , status       = Provisional
   , parser       = parseExprWith pExpr
   , similarity   = \a b -> cleanUpExpr a == cleanUpExpr b
   , equivalence  = viewEquivalent (polyViewWith rationalView)
   , isReady      = (`belongsTo` linearFactorsView)
   , strategy     = findFactorsStrategy
   , navigation   = exprNavigator
   , extraRules   = map liftToContext buggyRulesExpr
   , examples     = concat findFactors
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
        let norm = liftM ( normOrList cleanUpExpr 
                         . fmap toApprox 
                         . simplify quadraticEquationsView
                         ) . switch . fmap toEq
        in fromMaybe False $ liftM2 (==) (norm a) (norm b)
   | otherwise =
        equivalentRelation (viewEquivalent quadraticEquationsView) a b 
 where
   hasApprox = maybe False (any isApproximately) . disjunctions
   isApproximately = (==Approximately) . relationType
   toEq rel | relationType rel `elem` [EqualTo, Approximately] = 
      Just (leftHandSide rel :==: rightHandSide rel)
            | otherwise = Nothing
   toApprox (a :==: b) =
      let f x = case match doubleView x of
                   Just d  -> Number (precision 4 d)
                   Nothing -> x
      in f a .~=. f b
      
equivalentRelation :: (OrList (Equation a) -> OrList (Equation a) -> Bool) -> OrList (Relation a) -> OrList (Relation a) -> Bool
equivalentRelation f ra rb = fromMaybe False $ do
   a <- switch (fmap (match equationView) ra)
   b <- switch (fmap (match equationView) rb)
   return (f a b)

eqOrList :: (Relational f, Ord (f Expr)) => 
               (Expr -> Expr) -> OrList (f Expr) -> OrList (f Expr) -> Bool
eqOrList f x y = normOrList f x == normOrList f y

eqRelation :: (Relational f, Eq (f Expr)) => (Expr -> Expr) -> f Expr -> f Expr -> Bool
eqRelation f x y = fmap f x == fmap f y

-- Normalize the order of disjunctions. Simplify the expression with the function
-- passed as argument, but do not change (flip) the sides of the relation.
normOrList :: (Relational f, Ord (f Expr)) => 
                 (Expr -> Expr) -> OrList (f Expr) -> OrList (f Expr)
normOrList f = normalize . fmap (fmap (normExpr f))

normExpr :: (Expr -> Expr) -> Expr -> Expr
normExpr f = normalizeWith [plusOperator, timesOperator] . f
 where
   plusOperator  = acOperator (+) isPlus
   timesOperator = acOperator (*) isTimes