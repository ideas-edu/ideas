{-# LANGUAGE ExistentialQuantification, Rank2Types #-}
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
   ( exerciseList, findExercises, getExercise
   , openMathExercises, findOpenMathExercises, getOpenMathExercise
   , OpenMathExercise(..)
   , resolveExerciseCode
   ) where

import Common.Utils (Some(..))
import Common.Exercise
import Data.Char
import Domain.Math.Expr
import qualified Domain.LinearAlgebra as LA
import qualified Domain.Logic as Logic
import qualified Domain.RelationAlgebra as RA
import qualified Domain.Math.DerivativeExercise as Math
import qualified Domain.Math.Numeric.Exercises as Math
import qualified Domain.Math.Equation.CoverUpExercise as Math
import qualified Domain.Math.Polynomial.Exercises as Math

-- List with all known exercises
exerciseList :: [Some Exercise]
exerciseList = 
   [ -- logic and relation-algebra
     Some Logic.dnfExercise
   , Some Logic.dnfUnicodeExercise
   , Some RA.cnfExercise
   ] ++
   [ Some e | OMEX e <- openMathExercises ]

-----------------------------------------------------------------------------
-- All mathematical exercises are supported by the OpenMath standard, and 
-- require an extra type constraint

data OpenMathExercise = forall a . IsExpr a => OMEX (Exercise a)
   
openMathExercises :: [OpenMathExercise]
openMathExercises = 
   [ -- basic math
     OMEX Math.naturalExercise, OMEX Math.integerExercise
   , OMEX Math.rationalExercise, OMEX Math.fractionExercise
   , OMEX Math.coverUpExercise
   , OMEX Math.linearExercise
   , OMEX Math.quadraticExercise
   , OMEX Math.higherDegreeExercise
   , OMEX Math.derivativeExercise
     -- linear algebra
   , OMEX LA.gramSchmidtExercise
   , OMEX LA.linearSystemExercise
   , OMEX LA.gaussianElimExercise
   , OMEX LA.systemWithMatrixExercise
   ]
   
-----------------------------------------------------------------------------
-- Utility functions for finding an exercise

resolveExerciseCode :: Monad m => String -> m ExerciseCode
resolveExerciseCode txt = 
   case findExercises (\ex -> show (exerciseCode ex) ~= txt) of
      [Some ex] -> return (exerciseCode ex)
      _         -> fail $ "Failed to resolve the exercise code " ++ show txt
 where
   s ~= t = f s == f t 
   f = map toLower . filter isAlphaNum

findExercises :: (forall a . Exercise a -> Bool) -> [Some Exercise]
findExercises p = [ Some e | Some e <- exerciseList, p e ]

getExercise :: Monad m => ExerciseCode -> m (Some Exercise)
getExercise code = 
   case findExercises ((==code) . exerciseCode) of
      [hd] -> return hd
      []   -> fail $ "No exercise with code "   ++ show code
      _    -> fail $ "Ambiguous exercise code " ++ show code
      
findOpenMathExercises :: (forall a . Exercise a -> Bool) -> [OpenMathExercise]
findOpenMathExercises p = [ OMEX e | OMEX e <- openMathExercises, p e ]

getOpenMathExercise :: Monad m => ExerciseCode -> m OpenMathExercise
getOpenMathExercise code = 
   case findOpenMathExercises ((==code) . exerciseCode) of
      [hd] -> return hd
      []   -> fail $ "No exercise with code "   ++ show code
      _    -> fail $ "Ambiguous exercise code " ++ show code