module Domain.RelationAlgebra.Assignments where

import Domain.RelationAlgebra.Formula
import Domain.RelationAlgebra.Generator
import Domain.RelationAlgebra.Zipper
import Common.Assignment

cnfAssignment :: Assignment RelAlg
cnfAssignment = makeAssignment
   { shortTitle = "To conjunctive normal form"
--   , parser        :: String -> Either (Doc a, Maybe a) a
--   , prettyPrinter :: a -> String
--   , equivalence   :: a -> a -> Bool
--   , ruleset       :: [Rule a]
--   , strategy      :: LabeledStrategy a
   , generator = arbitrary
   }