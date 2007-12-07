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
   , module Domain.Logic.Checks
   , dnfAssignment
   ) where
   
import Domain.Logic.Formula
import Domain.Logic.Generator
import Domain.Logic.Zipper
import Domain.Logic.Parser
import Domain.Logic.Strategies
import Domain.Logic.Rules
import Domain.Logic.Checks

import Common.Assignment
import Common.Strategy
import Control.Monad
import System.Random

dnfAssignment :: Assignment LogicInContext
dnfAssignment = Assignment
   { parser        = \s -> case parseLogicPars s of
                              (p, [])   -> Right (inContext p)
                              (p, msgs) -> Left  (text (show msgs), Just (inContext p))
   , prettyPrinter = ppLogicPars . noContext
   , equivalence   = \x y -> noContext x `eqLogic` noContext y
   , equality      = (==)
   , finalProperty = isDNF . noContext
   , ruleset       = map liftLogicRule logicRules
   , strategy      = unlabel toDNF
   , generator     = let check p = not (isDNF p) && countEquivalences p < 2 && countBinaryOperators p <= 3
                     in liftM inContext (suitableLogic check)
   , configuration = defaultConfiguration
   }