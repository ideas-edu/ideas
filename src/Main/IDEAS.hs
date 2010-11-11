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
-----------------------------------------------------------------------------
module Main.IDEAS (useIDEAS) where

import Common.Rewriting
import Common.Utils (Some(..), fromShowString)
import Main.Options
import Service.DomainReasoner
import Service.ExercisePackage
import Service.ServiceList
import qualified Domain.LinearAlgebra as LA
import qualified Domain.LinearAlgebra.Checks as LA
import qualified Domain.Logic as Logic
import qualified Domain.Logic.FeedbackText as Logic
import qualified Domain.Math.Expr as Math
import qualified Domain.Math.Data.Interval as MathInterval
import qualified Domain.Math.Derivative.Exercises as Math
import qualified Domain.Math.Equation.CoverUpExercise as Math
import qualified Domain.Math.Numeric.Exercises as Math
import qualified Domain.Math.Numeric.Tests as MathNum
import qualified Domain.Math.Polynomial.Exercises as Math
import qualified Domain.Math.Polynomial.IneqExercises as Math
import qualified Domain.Math.Polynomial.RationalExercises as Math
import qualified Domain.Math.Polynomial.Tests as MathPoly
import qualified Domain.Math.Power.Exercises as Math
import qualified Domain.Math.Power.Equation.Exercises as Math
import qualified Domain.Math.SquareRoot.Tests as MathSqrt
import qualified Domain.Math.Polynomial.LeastCommonMultiple as MathLCM
-- import qualified Domain.RegularExpr.Exercises as RE
import qualified Domain.RelationAlgebra as RA

useIDEAS :: DomainReasoner a -> IO a
useIDEAS action = runDomainReasoner $ do
   -- version information
   setVersion     shortVersion
   setFullVersion fullVersion
   -- exercise packages
   addPackages    packages
   -- services
   addServices    serviceList
   addPkgService  exerciselistS
   -- domain checks
   addTestSuite $ do
      MathNum.main
      MathPoly.tests
      MathSqrt.tests
      MathInterval.testMe
      MathLCM.testLCM
      LA.checks
   -- do the rest
   action

packages :: [Some ExercisePackage]
packages =
   [ -- logic and relation-algebra
     Some (package Logic.dnfExercise)
        { withOpenMath    = True
        , toOpenMath      = termToOMOBJ . toTerm . fmap (Math.Var . fromShowString)
        , fromOpenMath    = (>>= fromTerm) . omobjToTerm
        , getExerciseText = Just logicText
        }
   , Some (package Logic.dnfUnicodeExercise)
        { withOpenMath    = True
        , toOpenMath      = termToOMOBJ . toTerm . fmap (Math.Var . fromShowString)
        , fromOpenMath    = (>>= fromTerm) . omobjToTerm
        , getExerciseText = Just logicText
        }
   -- , somePackage Logic.proofExercise
   , somePackage RA.cnfExercise
     -- basic math
   -- , someTermPackage Math.naturalExercise
   -- , someTermPackage Math.integerExercise
   -- , someTermPackage Math.rationalExercise
   , someTermPackage Math.fractionExercise
   , someTermPackage Math.coverUpExercise
   , someTermPackage Math.linearExercise
   , someTermPackage Math.linearMixedExercise
   , someTermPackage Math.quadraticExercise
   , someTermPackage Math.higherDegreeExercise
   , someTermPackage Math.findFactorsExercise
   , someTermPackage Math.ineqLinearExercise
   , someTermPackage Math.ineqQuadraticExercise
   , someTermPackage Math.ineqHigherDegreeExercise
   , someTermPackage Math.rationalEquationExercise
   , someTermPackage Math.simplifyRationalExercise
   -- , someTermPackage Math.divisionBrokenExercise
   , someTermPackage Math.quadraticNoABCExercise
   , someTermPackage Math.quadraticWithApproximation
   , someTermPackage Math.derivativeExercise
   , someTermPackage Math.derivativePolyExercise
   , someTermPackage Math.derivativeProductExercise
   , someTermPackage Math.derivativeQuotientExercise
   , someTermPackage Math.derivativePowerExercise
   , someTermPackage Math.simplifyPowerExercise
   , someTermPackage Math.powerOfExercise     
   , someTermPackage Math.nonNegBrokenExpExercise
   , someTermPackage Math.calcPowerExercise
   , someTermPackage Math.powerEqExercise
   , someTermPackage Math.expEqExercise
   , someTermPackage Math.logEqExercise
     -- linear algebra
   , someTermPackage LA.gramSchmidtExercise
   , someTermPackage LA.linearSystemExercise
   , someTermPackage LA.gaussianElimExercise
   , someTermPackage LA.systemWithMatrixExercise
     -- regular expressions
   -- , somePackage RE.regexpExercise
   ]
   
logicText :: ExerciseText Logic.SLogic
logicText = ExerciseText
   { ruleText              = Logic.ruleText
   , appliedRule           = Logic.appliedRule
   , feedbackSyntaxError   = Logic.feedbackSyntaxError
   , feedbackSame          = Logic.feedbackSame
   , feedbackBuggy         = Logic.feedbackBuggy
   , feedbackNotEquivalent = Logic.feedbackNotEquivalent
   , feedbackOk            = Logic.feedbackOk
   , feedbackDetour        = Logic.feedbackDetour
   , feedbackUnknown       = Logic.feedbackUnknown
   }