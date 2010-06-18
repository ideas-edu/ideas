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
-- Exercise for the logic domain: to prove two propositions equivalent
--
-----------------------------------------------------------------------------
module Domain.Logic.Proofs where

import Prelude hiding (repeat)
import Common.Context
import Common.Rewriting
import Common.Rewriting.Term (newSymbol, getConSpine, Symbol, binary)
import Common.Strategy hiding (fail)
import Common.Exercise
import Common.Utils (ShowString(..))
import Common.Transformation
import Common.Navigator
import Control.Monad
import Domain.Logic.Formula
import Domain.Logic.Generator (equalLogicA)
import Domain.Logic.Parser
import Domain.Logic.Examples 
import Domain.Logic.Strategies
import Domain.Math.Expr (useC, exprNavigator)

-- Currently, we use the DWA strategy
proofExercise :: Exercise (SLogic, SLogic)
proofExercise = makeExercise
   { exerciseId     = describe "Prove two propositions equivalent" $
                         newId "logic.proof"
   , status         = Experimental
   , parser         = parseLogicProof
   , prettyPrinter  = \(p, q) -> ppLogicPars p ++ " == " ++ ppLogicPars q
   , equivalence    = \(p, _) (r, s) -> eqLogic p r && eqLogic r s
   , similarity     = \(p, q) (r, s) -> equalLogicA p r && equalLogicA q s
   , isSuitable     = \(p, q) -> eqLogic p q
   , isReady        = \(p, q) -> isDNF p && isDNF q
   , strategy       = proofStrategy
   , navigation     = exprNavigator
   , examples       = exampleProofs
   }
   
instance (IsTerm a, IsTerm b) => IsTerm (a, b) where
   toTerm (a, b) = binary tupleSymbol (toTerm a) (toTerm b)
   fromTerm term =
      case getConSpine term of
         Just (s, [a, b]) | s == tupleSymbol ->
            liftM2 (,) (fromTerm a) (fromTerm b)
         _ -> fail "not a tuple"
   
tupleSymbol :: Symbol
tupleSymbol = newSymbol "basic.tuple"

proofStrategy :: LabeledStrategy (Context (SLogic, SLogic))
proofStrategy = label "proof equivalent" $ 
   onceLeft  (mapRules useC dnfStrategyDWA) <*>
   onceRight (mapRules useC dnfStrategyDWA)

onceLeft :: IsStrategy f => f (Context a) -> Strategy (Context a)
onceLeft s = ruleMoveDown <*> s <*> ruleMoveUp
 where
   ruleMoveDown = minorRule $ makeSimpleRuleList "MoveDown" (down 0)   
   ruleMoveUp   = minorRule $ makeSimpleRule "MoveUp" safeUp
   
   safeUp a = maybe (Just a) Just (up a)
   
onceRight :: IsStrategy f => f (Context a) -> Strategy (Context a)
onceRight s = ruleMoveDown <*> s <*> ruleMoveUp
 where
   ruleMoveDown = minorRule $ makeSimpleRuleList "MoveDown" (down 1)   
   ruleMoveUp   = minorRule $ makeSimpleRule "MoveUp" safeUp
   
   safeUp a = maybe (Just a) Just (up a)
   
go = printDerivation proofExercise (p :||: Not p, Not F)
 where p = Var (ShowString "p")