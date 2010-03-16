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
module Service.ExercisePackage
   ( -- Type, and selectors 
     ExercisePackage, exercise, withOpenMath
   , toOpenMath, fromOpenMath, getExerciseText
     -- Constructors
   , package, exprPackage, somePackage, someExprPackage
     -- Search functions
   , getPackage, getExercise
   ) where

import Common.Utils (Some(..))
import Common.Exercise
import Control.Monad
import Domain.Math.Expr (IsExpr(..), toOMOBJ, fromOMOBJ)
import Service.FeedbackText (ExerciseText)
import Text.OpenMath.Object

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

exprPackage :: IsExpr a => Exercise a -> ExercisePackage a
exprPackage ex = (package ex)
   { withOpenMath = True
   , toOpenMath   = toOMOBJ . toExpr
   , fromOpenMath = fromExpr . fromOMOBJ
   }

somePackage :: Exercise a -> Some ExercisePackage
somePackage = Some . package

someExprPackage :: IsExpr a => Exercise a -> Some ExercisePackage
someExprPackage = Some . exprPackage

-----------------------------------------------------------------------------
-- Utility functions for finding an exercise

getPackage :: Monad m => [Some ExercisePackage] -> ExerciseCode -> m (Some ExercisePackage)
getPackage pkgs code = 
   case filter p pkgs of
      [this] -> return this
      _      -> fail $ "Package " ++ show code ++ " not found" 
 where
   p (Some pkg) = exerciseCode (exercise pkg) == code
   
getExercise :: Monad m => [Some ExercisePackage] -> ExerciseCode -> m (Some Exercise)
getExercise pkgs = 
   let f (Some pkg) = Some (exercise pkg)
   in liftM f . getPackage pkgs