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
module Service.ExerciseList (packages, exercises) where

import Common.Utils (Some(..), fromShowString)
import Common.Exercise
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
        , toOpenMath      = toOMOBJ . toExpr . fmap (Var . fromShowString)
        , fromOpenMath    = exprToSLogic . fromOMOBJ
        , getExerciseText = Just Logic.logicText
        }
   , Some (package Logic.dnfUnicodeExercise)
        { withOpenMath    = True
        , toOpenMath      = toOMOBJ . toExpr . fmap (Var . fromShowString)
        , fromOpenMath    = exprToSLogic . fromOMOBJ
        , getExerciseText = Just Logic.logicText
        }
   , somePackage RA.cnfExercise
     -- basic math
   , someExprPackage Math.naturalExercise
   , someExprPackage Math.integerExercise
   , someExprPackage Math.rationalExercise
   , someExprPackage Math.fractionExercise
   , someExprPackage Math.coverUpExercise
   , someExprPackage Math.linearExercise
   , someExprPackage Math.quadraticExercise
   , someExprPackage Math.higherDegreeExercise
   , someExprPackage Math.findFactorsExercise
   , someExprPackage Math.ineqLinearExercise
   , someExprPackage Math.ineqQuadraticExercise
   , someExprPackage Math.ineqHigherDegreeExercise
   , someExprPackage Math.quadraticNoABCExercise
   , someExprPackage Math.quadraticWithApproximation
   , someExprPackage Math.derivativeExercise
   , someExprPackage Math.simplifyPowerExercise
   , someExprPackage Math.powerOfExercise     
   , someExprPackage Math.nonNegExpExercise
     -- linear algebra
   , someExprPackage LA.gramSchmidtExercise
   , someExprPackage LA.linearSystemExercise
   , someExprPackage LA.gaussianElimExercise
   , someExprPackage LA.systemWithMatrixExercise
     -- regular expressions
   , somePackage RE.regexpExercise
   ]

exercises :: [Some Exercise]
exercises = 
   let f (Some pkg) = Some (exercise pkg)
   in map f packages