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
   , higherPowerEqExercise
   , rootEqExercise
   ) where

import Prelude hiding ( (^) )

import Common.Library
import Data.Function (on)
import Domain.Math.Data.OrList
import Domain.Math.Data.Relation
import Domain.Math.Equation.Views
import Domain.Math.Power.Equation.Examples
import Domain.Math.Expr hiding (isPower)
import Domain.Math.Numeric.Views
import Domain.Math.Polynomial.Views
import Domain.Math.Power.Rules
import Domain.Math.Power.Utils (sortOrList)
import Domain.Math.Power.Equation.Strategies
import Domain.Math.Power.Equation.NormViews
import qualified Data.Foldable as F

------------------------------------------------------------
-- Exercises

powerEqExercise :: Exercise (Relation Expr)
powerEqExercise = let precision = 2 in makeExercise
  { status         = Provisional
  , parser         = parseRelExpr
  , strategy       = powerEqApproxStrategy
  , navigation     = termNavigator
  , exerciseId     = describe "solve power equation algebraically with x > 0" $ 
                       newId "algebra.manipulation.exponents.equation"
  , examples       = level Medium $ concatMap (map $ build equationView) $ 
                       powerEquations ++ [last higherPowerEquations]
  , ready          = predicate solvedRelation
  , suitable       = predicateView (normPowerEqApproxView precision)
  , equivalence    = withoutContext (viewEquivalent (normPowerEqApproxView precision))
  }
  
expEqExercise :: Exercise (Equation Expr)
expEqExercise = makeExercise
  { status         = Provisional
  , parser         = parseEqExpr
  , strategy       = expEqStrategy
  , navigation     = termNavigator
  , exerciseId     = describe "solve exponential equation algebraically" $ 
                       newId "algebra.manipulation.exponential.equation"
  , examples       = level Medium $ concat expEquations
  , ready          = predicate $ \ rel -> isVariable (leftHandSide rel) 
                           && rightHandSide rel `belongsTo` rationalView
  , suitable       = predicateView normExpEqView
  , equivalence    = withoutContext (viewEquivalent normExpEqView)
  , ruleOrdering   = ruleOrderingWithId [ getId root2power ]  
  }

logEqExercise :: Exercise (OrList (Relation Expr))
logEqExercise = makeExercise
  { status         = Provisional
  , parser         = parseOrsRelExpr
  , strategy       = logEqStrategy
  , navigation     = termNavigator
  , exerciseId     = describe "solve logarithmic equation algebraically" $ 
                       newId "algebra.manipulation.logarithmic.equation"
  , examples       = level Medium $ map (singleton . build equationView) (concat logEquations)
  , ready          = predicate solvedRelations
  , suitable       = predicateView (traverseView equationView >>> normLogEqView)
  , equivalence    = withoutContext (viewEquivalent (traverseView equationView >>> normLogEqView))
  , ruleOrdering   = ruleOrderingWithId [ getId calcPower
                                        , getId calcRoot ]
  }

higherPowerEqExercise :: Exercise (OrList (Equation Expr))
higherPowerEqExercise = makeExercise
  { status         = Provisional
  , parser         = parseOrsEqExpr
  , strategy       = higherPowerEqStrategy
  , navigation     = termNavigator
  , exerciseId     = describe "solve higher power equation algebraically" $ 
                       newId "algebra.manipulation.exponents.equation"
  , examples       = level Medium $ map singleton $ concat $ 
                       higherPowerEquations ++ take 3 rootEquations
  , ready          = predicate (solvedRelations)
  , suitable       = predicate (F.all (`belongsTo` normPowerEqView))
  , equivalence    = withoutContext (viewEquivalent ((normPowerEqView' hasSomeVar) >>> higherDegreeEquationsView))
  , ruleOrdering   = ruleOrderingWithId [ getId calcPower
                                        , getId calcRoot ]
  }

rootEqExercise :: Exercise (OrList (Equation Expr))
rootEqExercise = makeExercise
  { status         = Provisional
  , parser         = parseOrsEqExpr
  , strategy       = rootEqStrategy
  , navigation     = termNavigator
  , exerciseId     = describe "solve higher power equation algebraically" $ 
                       newId "algebra.manipulation.exponents.equation"
  , examples       = level Medium $ map singleton $ concat $ drop 3 rootEquations
  , ready          = predicate solvedRelations
  , suitable       = predicate (F.all (`belongsTo` normPowerEqView))
  , equivalence    = withoutContext (on (==) (sortOrList . simplify (normPowerEqView' $ elem "x" . vars)))
  , ruleOrdering   = ruleOrderingWithId [ getId calcPower
                                        , getId calcRoot ]
  }
