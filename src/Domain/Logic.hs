-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Domain.Logic
   ( module Domain.Logic.Formula
   , module Domain.Logic.Generator
   , module Domain.Logic.Zipper
   , module Domain.Logic.Parser
   , module Domain.Logic.Strategies
   , module Domain.Logic.Rules
   , dnfAssignment
   ) where
   
import Domain.Logic.Formula
import Domain.Logic.Generator
import Domain.Logic.Zipper
import Domain.Logic.Parser
import Domain.Logic.Strategies
import Domain.Logic.Rules

import Common.Assignment
import Common.Strategy
import Control.Monad
import System.Random

{- generator
* max. 1 equivalentie
* min. 4 stappen
* geen T/F in formule
* max. ?? stappen
-}

dnfAssignment :: Assignment LogicInContext
dnfAssignment = Assignment
   { shortTitle    = "Proposition to DNF" 
   , parser        = \s -> case parseLogicPars s of
                              (p, [])   -> Right (inContext p)
                              (p, msgs) -> Left  (text (show msgs), Just (inContext p))
   , prettyPrinter = ppLogicPars . noContext
   , equivalence   = \x y -> noContext x `eqLogic` noContext y
   , equality      = \x y -> noContext x == noContext y
   , finalProperty = isDNF . noContext
   , ruleset       = map liftLogicRule logicRules
   , strategy      = unlabel toDNF
   , generator     = let check p = not (isDNF p) && countEquivalences p < 2 && countBinaryOperators p <= 3
                     in liftM inContext generateLogic -- (suitableLogic check)
   , suitableTerm  = \p -> countEquivalences (noContext p) < 2 && countBinaryOperators (noContext p) <= 3
   , configuration = defaultConfiguration
   }