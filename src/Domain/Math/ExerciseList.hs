-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- All exported exercises in the mathematical domain
--
-----------------------------------------------------------------------------
module Domain.Math.ExerciseList 
   ( exerciseList, scriptList, testSuiteList
   ) where

import Common.Exercise
import Common.Id
import Common.TestSuite
import Common.Utils (Some(..))
import Domain.Math.Data.Interval
import Domain.Math.Derivative.Exercises
import Domain.Math.Equation.CoverUpExercise
import Domain.Math.Numeric.Exercises
import Domain.Math.Polynomial.Balance
import Domain.Math.Polynomial.Exercises
import Domain.Math.Polynomial.IneqExercises
import Domain.Math.Polynomial.LeastCommonMultiple
import Domain.Math.Polynomial.RationalExercises
import Domain.Math.Power.Equation.Exercises
import Domain.Math.Power.Exercises
import qualified Domain.Math.Numeric.Tests as MathNum
import qualified Domain.Math.Polynomial.Tests as MathPoly
import qualified Domain.Math.SquareRoot.Tests as MathSqrt

exerciseList :: [Some Exercise]
exerciseList =
   [ -- basic math
   -- , Some naturalExercise
   -- , Some integerExercise
   -- , Some rationalExercise
     Some fractionExercise
   , Some coverUpExercise
   , Some linearExercise
   , Some linearMixedExercise
   , Some balanceExercise
   , Some quadraticExercise
   , Some higherDegreeExercise
   , Some findFactorsExercise
   , Some expandExercise
   , Some ineqLinearExercise
   , Some ineqQuadraticExercise
   , Some ineqHigherDegreeExercise
   , Some rationalEquationExercise
   , Some simplifyRationalExercise
   -- , Some divisionBrokenExercise
   , Some quadraticNoABCExercise
   , Some quadraticWithApproximation
   , Some derivativeExercise
   , Some derivativePolyExercise
   , Some derivativeProductExercise
   , Some derivativeQuotientExercise
   -- , Some derivativePowerExercise
   , Some simplifyPowerExercise
   , Some powerOfExercise     
   , Some nonNegBrokenExpExercise
   , Some calcPowerExercise
   , Some powerEqExercise
   , Some expEqExercise
   , Some logEqExercise
--   , Some higherPowerEqExercise
   ]
   
scriptList :: [(Id, FilePath)]
scriptList =
   [ (getId linearExercise,       "math.lineq-en.txt")
   , (getId quadraticExercise,    "math.quadreq-en.txt")
   , (getId higherDegreeExercise, "math.polyeq-en.txt")
   ]      

testSuiteList :: [TestSuite]
testSuiteList = [MathNum.main, MathPoly.tests, MathSqrt.tests, testMe, testLCM]