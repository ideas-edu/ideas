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
module Main.ExerciseList (packages, useIDEAS) where

import Common.Utils (Some(..), fromShowString)
import Common.Rewriting
import Domain.Math.Expr
import Service.ExercisePackage
import Service.FeedbackText
import Service.DomainReasoner
import qualified Domain.LinearAlgebra as LA
import qualified Domain.Logic as Logic
import qualified Domain.Logic.FeedbackText as Logic
import qualified Domain.RelationAlgebra as RA
import qualified Domain.Math.DerivativeExercise as Math
import qualified Domain.Math.Numeric.Exercises as Math
import qualified Domain.Math.Equation.CoverUpExercise as Math
import qualified Domain.Math.Polynomial.Exercises as Math
import qualified Domain.Math.Polynomial.IneqExercises as Math
import qualified Domain.RegularExpr.Exercises as RE
import qualified Domain.Math.Power.Exercises as Math
import Main.Options
import Service.ServiceList

packages :: [Some ExercisePackage]
packages =
   [ -- logic and relation-algebra
     Some (package Logic.dnfExercise)
        { withOpenMath    = True
        , toOpenMath      = termToOMOBJ . toTerm . fmap (Var . fromShowString)
        , fromOpenMath    = (>>= fromTerm) . omobjToTerm
        , getExerciseText = Just logicText
        }
   , Some (package Logic.dnfUnicodeExercise)
        { withOpenMath    = True
        , toOpenMath      = termToOMOBJ . toTerm . fmap (Var . fromShowString)
        , fromOpenMath    = (>>= fromTerm) . omobjToTerm
        , getExerciseText = Just logicText
        }
   , somePackage RA.cnfExercise
     -- basic math
   , someTermPackage Math.naturalExercise
   , someTermPackage Math.integerExercise
   , someTermPackage Math.rationalExercise
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
   , someTermPackage Math.quadraticNoABCExercise
   , someTermPackage Math.quadraticWithApproximation
   , someTermPackage Math.derivativeExercise
   , someTermPackage Math.simplifyPowerExercise
   , someTermPackage Math.powerOfExercise     
   , someTermPackage Math.nonNegExpExercise
   , someTermPackage Math.calcPowerExercise
     -- linear algebra
   , someTermPackage LA.gramSchmidtExercise
   , someTermPackage LA.linearSystemExercise
   , someTermPackage LA.gaussianElimExercise
   , someTermPackage LA.systemWithMatrixExercise
     -- regular expressions
   , somePackage RE.regexpExercise
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
   
useIDEAS :: DomainReasoner a -> IO a
useIDEAS action = runDomainReasoner $ do
   setVersion     shortVersion
   setFullVersion fullVersion
   addPackages    packages
   addServices    serviceList
   addPkgService  exerciselistS
   action