-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  unknown
--
--
-----------------------------------------------------------------------------

module Domain.Programming.Exercises where

import Common.Apply
import Common.Context
import Common.Exercise hiding (checkExercise)
import Common.Rewriting
import Common.Strategy hiding (fail, not)
import Control.Monad (unless, fail)
import Data.Maybe
import Data.List
import Domain.Programming.AlphaRenaming (alphaRenaming)
import Domain.Programming.Strategies
import Domain.Programming.HeliumRules
import Domain.Programming.Helium
import Domain.Programming.Prog
import Domain.Programming.PreludeS
import Domain.Programming.EncodingExercises
import Prelude hiding (fail)
import Text.Parsing (SyntaxError(..))

heliumExercise :: Exercise Module
heliumExercise = Exercise   
   { identifier    = "helium"
   , domain        = "programming"
   , description   = "Flexible fromBin strategy"
   , status        = Experimental
   , parser        = \s -> if s == "" 
                           then Right emptyProg 
                           else  case compile s of
                                   Left e  -> Left $ ErrorMessage e
                                   Right m -> Right m
   , subTerm       = \_ _ -> Nothing
   , prettyPrinter = ppModule
   , equivalence   = \_ _ -> True
   , equality      = equalModules
   , finalProperty = const True
   , ruleset       = []
   , strategy      = label "fromBin :: [Int] -> Int" fromBinStrategy
   , differences   = \_ _ -> [([], Different)]
   , ordering      = \_ _ -> LT
   , termGenerator = makeGenerator (const True) (return emptyProg)
   }

toDecExercises :: [Exercise Module]
toDecExercises = map (\ex -> heliumExercise { strategy = label "helium" (stringToStrategy ex) }) toDecs

fromBinExercises :: [Exercise Module]
fromBinExercises = map (\ex -> heliumExercise { strategy = label "fromBin" (stringToStrategy ex)
                                              , description = "Student solutions fromBin" 
                                              }) fromBins

{-
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
-}
