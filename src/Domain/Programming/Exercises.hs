module Domain.Programming.Exercises where

import Domain.Programming.Expr
import Domain.Programming.Parser
import Domain.Programming.Strategies
import Common.Context
import Common.Strategy
import Common.Uniplate
import Common.Exercise
import Common.Transformation
import Common.Apply
import Common.Parsing (SyntaxError(..))
import Data.Maybe
import Data.Char


isortExercise :: Exercise Expr
isortExercise = Exercise   
   { identifier    = "isort"
   , domain        = "programming"
   , description   = "Insertion sort"
   , status        = Experimental
   , parser        = \s -> case reads s of  
                             [(a, rest)] | all isSpace rest -> Right a 
                             _ -> Left $ ErrorMessage "parse error"
   , subTerm       = \_ _ -> Nothing
   , prettyPrinter = \e -> ppExpr (e,0)
   , equivalence   = (==)
   , equality      = (==)
   , finalProperty = const True
   , ruleset       = []
   , strategy      = label "isort" isortAbstractStrategy
   , generator     = return undef
   , suitableTerm  = const True
   }
