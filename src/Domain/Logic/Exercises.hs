-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (...add description...)
--
-----------------------------------------------------------------------------
module Domain.Logic.Exercises where

import Domain.Logic.Generator
import Domain.Logic.Formula
import Domain.Logic.Strategies
import Domain.Logic.Parser
import Domain.Logic.Rules
import Common.Exercise
import Common.Strategy hiding (not)
import Common.Context
import Common.Parsing (fromRanged, subExpressionAt)
import Control.Monad
import Data.Maybe
{-
import Test.QuickCheck hiding (check)
import Data.List -}
{- generator
* max. 1 equivalentie
* min. 4 stappen (dus niet in DNF)
* geen T/F in formule
* max. ?? stappen
-}

dnfExercise :: Exercise Logic
dnfExercise = standard
   { shortTitle    = "Proposition to DNF" 
   , parser        = \s -> case parseLogicPars s of
                              (p, [])      -> Right (fromRanged p)
                              (_, (a,b):_) -> Left $ "Parse error" ++ 
                                              maybe "" (\x -> " on " ++ show x) b ++ ":\n   expecting " ++ show a
   , subTerm       = \s r -> case parseLogicPars s of
                                (p, []) -> fmap makeLocation (subExpressionAt r p)
                                _       -> Nothing
   , prettyPrinter = ppLogicPars
   , equivalence   = eqLogic
   , equality      = equalLogicAC
   , finalProperty = isDNF
   , ruleset       = map liftRuleToContext logicRules
   , strategy      = toDNFDWA
   , generator     = generateLogic
   , suitableTerm  = \p -> let n = stepsRemaining (emptyPrefix toDNF) (inContext p)
                           in countEquivalences p < 2 && n >= 4 && n <= 12
   }
 where
   standard :: Exercise (Context Logic)
   standard = makeExercise