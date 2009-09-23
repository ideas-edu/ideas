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
-- An exercise for the programming domain.
-- 
-----------------------------------------------------------------------------

module Domain.Programming.Exercises where

import Common.Exercise
import Common.Rewriting
import Common.Strategy
import Domain.Programming.Strategies
import Domain.Programming.HeliumRules
import Domain.Programming.Helium
import Domain.Programming.Prog
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
   , equality      = equalModules ["fromBin"]
   , finalProperty = const True
   , ruleset       = []
   , strategy      = label "fromBin :: [Int] -> Int" fromBinStrategy
   , differences   = \_ _ -> [([], Different)]
   , ordering      = \_ _ -> LT
   , termGenerator = makeGenerator (const True) (return emptyProg)
   }
