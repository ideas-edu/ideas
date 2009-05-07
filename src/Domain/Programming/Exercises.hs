module Domain.Programming.Exercises where

import qualified Domain.Programming.Expr as E
import Domain.Programming.Expr hiding (undef)
import Domain.Programming.Parser
import Domain.Programming.Strategies
import Domain.Programming.HeliumRules
import Domain.Programming.Helium
import Common.Context
import Common.Strategy
import Common.Uniplate
import Common.Exercise
import Common.Transformation
import Common.Apply
import Text.Parsing (SyntaxError(..))
import Common.Rewriting
import Data.Maybe
import Data.Char
import Domain.Programming.Parser
import Domain.Programming.Helium
import qualified Domain.Programming.Helium as H
import qualified UHA_Pretty as PP (sem_Module) 

import Data.Generics.Biplate
import Data.Generics.PlateData
import Data.Data hiding (Fixity)

isortExercise :: Exercise Expr
isortExercise = Exercise   
   { identifier    = "isort"
   , domain        = "programming"
   , description   = "Insertion sort"
   , status        = Experimental
{-   , parser        = \s -> case reads s of  
                             [(a, rest)] | all isSpace rest -> Right a 
                             _ -> Left $ ErrorMessage "parse error" -}
   , parser        = parseExpr
   , subTerm       = \_ _ -> Nothing
   , prettyPrinter = \e -> ppExpr (e,0)
   , equivalence   = \_ _ -> True
   , equality      = (==)
   , finalProperty = const True
   , ruleset       = []
   , strategy      = label "isort"  isortAbstractStrategy
   , differences   = treeDiff
   , ordering      = compare
   , termGenerator = makeGenerator (const True) (return E.undef)
   }

heliumExercise :: Exercise Module
heliumExercise = Exercise   
   { identifier    = "helium"
   , domain        = "programming"
   , description   = "Helium testing"
   , status        = Experimental
   , parser        = \s -> if s == "" then Right emptyProg else modParser s 
   , subTerm       = \_ _ -> Nothing
   , prettyPrinter = show . PP.sem_Module
   , equivalence   = \_ _ -> True
   , equality      = \x y -> (transformBi (\(Range_Range  _ _) -> noRange) x) == y
   , finalProperty = const True
   , ruleset       = []
   , strategy      = label "helium" sumStrategy'
   , differences   = \_ _ -> [([], Different)]
   , ordering      = \_ _ -> LT
   , termGenerator = makeGenerator (const True) (return emptyProg)
   }

modParser s = case compile s of
                Left e  -> Left $ ErrorMessage e
                Right m -> Right m

