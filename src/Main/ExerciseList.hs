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
module Main.ExerciseList (packages, exercises) where

import Common.Utils (Some(..), fromShowString)
import Common.Exercise
import Common.Rewriting
import Domain.Math.Expr
import Service.ExercisePackage
import qualified Domain.LinearAlgebra as LA
import qualified Domain.Logic as Logic
import qualified Domain.RelationAlgebra as RA
import qualified Domain.Math.DerivativeExercise as Math
import qualified Domain.Math.Numeric.Exercises as Math
import qualified Domain.Math.Equation.CoverUpExercise as Math
import qualified Domain.Math.Polynomial.Exercises as Math
import qualified Domain.Math.Polynomial.IneqExercises as Math
import qualified Domain.RegularExpr.Exercises as RE
import qualified Domain.Math.Power.Exercises as Math

packages :: [Some ExercisePackage]
packages =
   [ -- logic and relation-algebra
     Some (package Logic.dnfExercise)
        { withOpenMath    = True
        , toOpenMath      = termToOMOBJ . toTerm . fmap (Var . fromShowString)
        , fromOpenMath    = (>>= fromTerm) . omobjToTerm
        , getExerciseText = Just Logic.logicText
        }
   , Some (package Logic.dnfUnicodeExercise)
        { withOpenMath    = True
        , toOpenMath      = termToOMOBJ . toTerm . fmap (Var . fromShowString)
        , fromOpenMath    = (>>= fromTerm) . omobjToTerm
        , getExerciseText = Just Logic.logicText
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

exercises :: [Some Exercise]
exercises = 
   let f (Some pkg) = Some (exercise pkg)
   in map f packages