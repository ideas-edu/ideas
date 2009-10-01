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
import Test.QuickCheck hiding (defaultConfig, label)
import Text.Parsing (SyntaxError(..))

heliumExercise :: Exercise Module
heliumExercise = makeExercise   
   { exerciseCode   = makeCode "programming" "helium"
   , description    = "Flexible fromBin strategy"
   , status         = Experimental
   , parser         = \s -> if s == "" 
                            then Right emptyProg 
                            else  case compile s of
                                    Left e  -> Left $ ErrorMessage e
                                    Right m -> Right m
   , prettyPrinter  = ppModule
   , equivalence    = \_ _ -> True
   , similarity     = equalModules ["fromBin"]
   , isReady        = const True
   , isSuitable     = const True
   , extraRules     = []
   , strategy       = label "fromBin :: [Int] -> Int" fromBinStrategy
   , differences    = \_ _ -> [([], Different)]
   , testGenerator  = Just arbitrary
   , randomExercise = useGenerator (const True) (return emptyProg)
   }

instance Arbitrary Module where
  arbitrary = return emptyProg