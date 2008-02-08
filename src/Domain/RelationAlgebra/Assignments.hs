module Domain.RelationAlgebra.Assignments where

import Domain.RelationAlgebra.Formula
import Domain.RelationAlgebra.Generator
import Domain.RelationAlgebra.Zipper
import Domain.RelationAlgebra.Strategies
import Domain.RelationAlgebra.Rules
import Domain.RelationAlgebra.Parser
import Common.Transformation
import Common.Assignment
import Control.Monad
import Test.QuickCheck

cnfAssignment :: Assignment RelAlgInContext
cnfAssignment = makeAssignment
   { shortTitle = "To conjunctive normal form"

   , parser        = \s -> case parseRelAlg s of
                              (p, [])   -> Right (inContext p)
                              (p, msgs) -> Left  (text (show msgs), Just (inContext p))
   , prettyPrinter = ppRelAlg . noContext
--   , equivalence = \x y -> apply toCNF (inContext $ noContext x) == apply toCNF (inContext $ noContext y)
   , ruleset   = map liftRelAlgRule relAlgRules
   , strategy  = toCNF
   , generator = liftM inContext arbitrary
   , suitableTerm = not . isCNF . noContext
   }
   
instance Arbitrary a => Arbitrary (Loc a) where
   arbitrary = liftM (Loc Top) arbitrary
   coarbitrary (Loc _ a) = coarbitrary a
