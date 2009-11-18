{-# LANGUAGE Rank2Types #-}
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
module Service.ExerciseList 
   ( ExercisePackage, exercise, withOpenMath, toOpenMath, fromOpenMath
   , getExerciseText
   , package, packages, exercises
   , getPackage, getExercise
   ) where

import Common.Utils (Some(..))
import Common.Exercise
import Control.Monad
import Domain.Math.Expr
import Service.FeedbackText (ExerciseText)
import Text.OpenMath.Object
import qualified Domain.LinearAlgebra as LA
import qualified Domain.Logic as Logic
import qualified Domain.RelationAlgebra as RA
import qualified Domain.Math.DerivativeExercise as Math
import qualified Domain.Math.Numeric.Exercises as Math
import qualified Domain.Math.Equation.CoverUpExercise as Math
import qualified Domain.Math.Polynomial.Exercises as Math
import qualified Domain.Math.Polynomial.IneqExercises as Math
import qualified Domain.RegularExpr.Exercises as RE

packages :: [Some ExercisePackage]
packages =
   [ -- logic and relation-algebra
     Some (package Logic.dnfExercise)
        {getExerciseText = Just Logic.logicText}
   , Some (package Logic.dnfUnicodeExercise)
        {getExerciseText = Just Logic.logicText}
   , make RA.cnfExercise
     -- basic math
   , makeOM Math.naturalExercise, makeOM Math.integerExercise
   , makeOM Math.rationalExercise, makeOM Math.fractionExercise
   , makeOM Math.coverUpExercise
   , makeOM Math.linearExercise
   , makeOM Math.quadraticExercise
   , makeOM Math.higherDegreeExercise
   , makeOM Math.ineqLinearExercise
   , makeOM Math.ineqQuadraticExercise
   , makeOM Math.ineqHigherDegreeExercise
   , makeOM Math.quadraticNoABCExercise
   , makeOM Math.quadraticWithApproximation
   , makeOM Math.derivativeExercise
     -- linear algebra
   , makeOM LA.gramSchmidtExercise, makeOM LA.linearSystemExercise
   , makeOM LA.gaussianElimExercise, makeOM LA.systemWithMatrixExercise
     -- regular expressions
   , make RE.regexpExercise
   ]

-----------------------------------------------------------------------------
-- Package data type (and constructor functions)

data ExercisePackage a = P
   { exercise        :: Exercise a
   , withOpenMath    :: Bool
   , toOpenMath      :: a -> OMOBJ 
   , fromOpenMath    :: MonadPlus m => OMOBJ -> m a
   , getExerciseText :: Maybe (ExerciseText a)
   }

package :: Exercise a -> ExercisePackage a
package ex = P 
   { exercise        = ex
   , withOpenMath    = False
   , toOpenMath      = error "no OpenMath support"
   , fromOpenMath    = fail "no OpenMath support"
   , getExerciseText = Nothing
   }

make :: Exercise a -> Some ExercisePackage
make = Some . package

makeOM :: IsExpr a => Exercise a -> Some ExercisePackage
makeOM ex = Some $ (package ex)
   { withOpenMath = True
   , toOpenMath   = toOMOBJ . toExpr
   , fromOpenMath = fromExpr . fromOMOBJ
   }

-----------------------------------------------------------------------------
-- Utility functions for finding an exercise

exercises :: [Some Exercise]
exercises = 
   let f (Some pkg) = Some (exercise pkg)
   in map f packages

getPackage :: Monad m => ExerciseCode -> m (Some ExercisePackage)
getPackage code = 
   case filter p packages of
      [this] -> return this
      _      -> fail $ "Package " ++ show code ++ " not found" 
 where
   p (Some pkg) = exerciseCode (exercise pkg) == code
   
getExercise :: Monad m => ExerciseCode -> m (Some Exercise)
getExercise = 
   let f (Some pkg) = Some (exercise pkg)
   in liftM f . getPackage