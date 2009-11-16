-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Domain.Math.Polynomial.IneqExercises 
   ( ineqLinearExercise, ineqQuadraticExercise, ineqHigherDegreeExercise
   ) where

import Common.Exercise
import Common.View
import Domain.Math.Data.Relation
import Domain.Math.Expr
import Domain.Math.Examples.DWO2

ineqLinearExercise :: Exercise (Relation Expr)
ineqLinearExercise = makeExercise 
   { description  = "solve a linear inequation"
   , exerciseCode = makeCode "math" "linineq"
   , examples     = map (build inequalityView) (concat ineqLin1)
   }
   
ineqQuadraticExercise :: Exercise (Relation Expr)
ineqQuadraticExercise = makeExercise 
   { description  = "solve a quadratic inequation"
   , exerciseCode = makeCode "math" "quadrineq"
   , examples     = map (build inequalityView) (concat $ ineqQuad1 ++ [ineqQuad2])
   }

ineqHigherDegreeExercise :: Exercise (Relation Expr)
ineqHigherDegreeExercise = makeExercise 
   { description  = "solve an inequation of higher degree"
   , exerciseCode = makeCode "math" "ineqhigherdegree"
   , examples     = map (build inequalityView) ineqHigh
   }