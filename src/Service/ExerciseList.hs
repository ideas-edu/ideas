module Service.ExerciseList (exerciseList) where

import Common.Utils (Some(..))
import Common.Exercise
import qualified Domain.LinearAlgebra as LA
import qualified Domain.Logic as Logic
import qualified Domain.RelationAlgebra as RA
import qualified Domain.Math.DerivativeExercise as Math
import qualified Domain.Math.Fraction as Math
import qualified Domain.Fraction as Math
import qualified Domain.Math.LinearEquations as Math
import qualified Domain.Math.HigherDegreeEquations as Math

exerciseList :: [Some Exercise]
exerciseList = 
   [ -- linear algebra
     Some LA.reduceMatrixExercise
   , Some LA.solveSystemExercise
   , Some LA.solveSystemWithMatrixExercise
   , Some LA.solveGramSchmidt
     -- basic math
   , Some Math.derivativeExercise
   , Some Math.simplExercise
   , Some Math.fractionExercise
   , Some Math.linearEquationExercise
   , Some Math.higherDegreeEquationExercise
     -- logic and relation-algebra
   , Some Logic.dnfExercise
   , Some RA.cnfExercise
   ]