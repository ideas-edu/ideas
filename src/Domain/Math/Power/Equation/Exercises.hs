-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Domain.Math.Power.Equation.Exercises    
   ( powerEqExercise
   , expEqExercise
   , logEqExercise
   ) where

import Prelude hiding ( (^) )

import Common.Context
import Common.Exercise
import Common.Strategy hiding (not, replicate)
import Common.View
import Domain.Math.Data.OrList
import Domain.Math.Data.Relation
import Domain.Math.Equation.CoverUpExercise
import Domain.Math.Equation.Views
import Domain.Math.Examples.DWO4 ( powerEquations, expEquations, logEquations
                                 , higherPowerEquations)
import Domain.Math.Expr hiding (isPower)
import Domain.Math.Numeric.Views
import Domain.Math.Polynomial.CleanUp
import Domain.Math.Power.Rules
import Domain.Math.Power.Equation.Strategies
import Domain.Math.Power.Equation.NormViews


------------------------------------------------------------
-- Exercises

powerEqExercise :: Exercise (Relation Expr)
powerEqExercise = makeExercise
  { status         = Provisional
  , parser         = parseExprWith (pRelation pExpr)
  , strategy       = powerEqApproxStrategy
  , navigation     = termNavigator
  , exerciseId     = describe "solve power equation algebraically with x > 0" $ 
                       newId "algebra.manipulation.exponents.equation"
  , examples       = concatMap (map (build equationView)) powerEquations
  , isReady        = \ rel -> isVariable (leftHandSide rel) 
                           && case rightHandSide rel of
                                Number _ -> True
                                _        -> False
  , isSuitable   = (`belongsTo` (normPowerEqApproxView 2))
  , equivalence    = viewEquivalent (normPowerEqApproxView 2)
  }
  
expEqExercise :: Exercise (Equation Expr)
expEqExercise = makeExercise
  { status         = Provisional
  , parser         = parseExprWith (pEquation pExpr)
  , strategy       = expEqStrategy
  , navigation     = termNavigator
  , exerciseId     = describe "solve exponential equation algebraically" $ 
                       newId "algebra.manipulation.exponential.equation"
  , examples       = concat expEquations
  , isReady        = \ rel -> isVariable (leftHandSide rel) 
                           && rightHandSide rel `belongsTo` rationalView
  , isSuitable     = (`belongsTo` normExpEqView)
  , equivalence    = viewEquivalent normExpEqView
  }

logEqExercise :: Exercise (OrList (Relation Expr))
logEqExercise = makeExercise
  { status         = Provisional
  , parser         = parseExprWith (pOrList (pRelation pExpr))
  , strategy       = logEqStrategy
  , navigation     = termNavigator
  , exerciseId     = describe "solve logarithmic equation algebraically" $ 
                       newId "algebra.manipulation.logarithmic.equation"
  , examples       = map (orList . return . build equationView) (concat logEquations)
  , isReady        = solvedRelations
  , isSuitable     = (`belongsTo` (switchView equationView >>> normLogEqView))
  , equivalence    = viewEquivalent (switchView equationView >>> normLogEqView)
  , ruleOrdering   = ruleOrderingWithId [ getId calcPower
                                        , getId calcRoot ]
  }

-- higherPowerEquationExercise :: Exercise (OrList (Relation Expr))
-- higherPowerEquationExercise = makeExercise
--   { status         = Provisional
--   , parser         = parseExprWith (pOrList (pRelation pExpr))
--   , strategy       = coverUpStrategy--higherPowerEqStrategy
--   , navigation     = termNavigator
--   , exerciseId     = describe "solve higher power equation algebraically" $ 
--                        newId "algebra.manipulation.exponents.equation"
--   , examples       = map (orList . return . build equationView) $
--                       concat $ init higherPowerEquations
--   , isReady        = solvedRelations
--   -- , isSuitable     = (`belongsTo` (switchView equationView >>> normPowerEqView))
--   -- , equivalence    = viewEquivalent (switchView equationView >>> normPowerEqView)
--   , ruleOrdering   = ruleOrderingWithId [ getId calcPower
--                                         , getId calcRoot ]
--   }

higherPowerEqExercise :: Exercise (OrList (Equation Expr))
higherPowerEqExercise = makeExercise
  { status         = Provisional
  , parser         = parseExprWith (pOrList (pEquation pExpr))
  , strategy       = cleanUpStrategy (applyTop $ fmap (fmap cleanUpExpr)) coverUpStrategy
  , navigation     = termNavigator
  , exerciseId     = describe "solve higher power equation algebraically" $ 
                       newId "algebra.manipulation.exponents.equation"
  , examples       = map (orList . return) $ concat $ init higherPowerEquations
  , isReady        = solvedRelations
  -- , isSuitable     = (`belongsTo` (switchView equationView >>> normPowerEqView))
  -- , equivalence    = viewEquivalent (switchView equationView >>> normPowerEqView)
  , ruleOrdering   = ruleOrderingWithId [ getId calcPower
                                        , getId calcRoot ]
  }

