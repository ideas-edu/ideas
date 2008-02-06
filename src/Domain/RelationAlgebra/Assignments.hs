module Domain.RelationAlgebra.Assignments where

import Domain.RelationAlgebra.Formula
import Domain.RelationAlgebra.Generator
import Common.Assignment

cnfAssignment :: Assignment RelAlg
cnfAssignment = makeAssignment
   { shortTitle = "To conjunctive normal form"
--   , parser        :: String -> Either (Doc a, Maybe a) a
--   , prettyPrinter :: a -> String
--   , equivalence   :: a -> a -> Bool
--   , equality      :: a -> a -> Bool -- syntactic equality
--   , ruleset       :: [Rule a]
--   , strategy      :: LabeledStrategy a
   }