module Domain.Logic.Exercises where
import Domain.Logic.Generator
import Domain.Logic.Formula
import Domain.Logic.Strategies
import Domain.Logic.Parser
import Domain.Logic.Rules

import Common.Exercise
import Common.Strategy hiding (not)
import Common.Context
import Common.Parsing
import Control.Monad

{- generator
* max. 1 equivalentie
* min. 4 stappen
* geen T/F in formule
* max. ?? stappen
-}

dnfExercise :: Exercise (Context Logic)
dnfExercise = Exercise
   { shortTitle    = "Proposition to DNF" 
   , parser        = \s -> case parseLogicPars s of
                              (p, [])   -> Right (inContext (fromRanged p))
                              (p, msgs) -> Left  (text (show msgs), Just (inContext (fromRanged p)))
   , prettyPrinter = ppLogicPars . fromContext
   , equivalence   = \x y -> fromContext x `eqLogic` fromContext y
   , equality      = \x y -> fromContext x == fromContext y
   , finalProperty = isDNF . fromContext
   , ruleset       = map liftRuleToContext logicRules
   , strategy      = toDNF
   , generator     = let check p = not (isDNF p) && countEquivalences p < 2 && countBinaryOperators p <= 3
                     in liftM inContext generateLogic -- (suitableLogic check)
   , suitableTerm  = \p -> countEquivalences (fromContext p) < 2 && countBinaryOperators (fromContext p) <= 3
   }