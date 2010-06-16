{-# LANGUAGE Rank2Types #-}
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
module Service.ExercisePackage
   ( -- Type, and selectors 
     ExercisePackage, exercise, withOpenMath
   , toOpenMath, fromOpenMath, getExerciseText
     -- Constructors
   , package, termPackage, somePackage, someTermPackage
     -- Conversion functions to/from OpenMath
   , termToOMOBJ, omobjToTerm
     -- ExerciseText datatype
   , ExerciseText(..)
   ) where

import Common.Context (Context)
import Common.Transformation (Rule)
import Common.Utils (Some(..))
import Common.Exercise
import Control.Monad
import Common.Rewriting.Term hiding (Symbol)
import Data.Char
import Data.List
import Text.OpenMath.Object
import Text.OpenMath.Symbol
import Text.OpenMath.Dictionary.Fns1

-----------------------------------------------------------------------------
-- Package data type (and constructor functions)

data ExercisePackage a = P
   { exercise        :: Exercise a
   , withOpenMath    :: Bool
   , toOpenMath      :: a -> OMOBJ 
   , fromOpenMath    :: MonadPlus m => OMOBJ -> m a
   , getExerciseText :: Maybe (ExerciseText a)
   }

instance HasId (ExercisePackage a) where
   getId = getId . exercise
   changeId f pkg = pkg { exercise = changeId f (exercise pkg) }

package :: Exercise a -> ExercisePackage a
package ex = P 
   { exercise        = ex
   , withOpenMath    = False
   , toOpenMath      = error "no OpenMath support"
   , fromOpenMath    = fail "no OpenMath support"
   , getExerciseText = Nothing
   }

termPackage :: IsTerm a => Exercise a -> ExercisePackage a
termPackage ex = (package ex)
   { withOpenMath = True
   , toOpenMath   = termToOMOBJ . toTerm
   , fromOpenMath = (>>= fromTerm) . omobjToTerm
   }

somePackage :: Exercise a -> Some ExercisePackage
somePackage = Some . package

someTermPackage :: IsTerm a => Exercise a -> Some ExercisePackage
someTermPackage = Some . termPackage
   
-----------------------------------------------------------------------------
-- Utility functions for conversion to/from OpenMath

termToOMOBJ :: Term -> OMOBJ
termToOMOBJ term =
   case term of
      Var s   -> OMV s
      Con s   -> OMS (idToSymbol (getId s))
      Meta i  -> OMV ("$" ++ show i)
      Num n   -> OMI n
      Float d -> OMF d
      App _ _ -> let (f, xs) = getSpine term
                 in make (map termToOMOBJ (f:xs))
 where
   make [OMS s, OMV x, body] | s == lambdaSymbol = 
      OMBIND (OMS s) [x] body
   make xs = OMA xs

omobjToTerm :: MonadPlus m => OMOBJ -> m Term
omobjToTerm omobj =
   case omobj of 
      OMV x -> case isMeta x of
                  Just n  -> return (Meta n)
                  Nothing -> return (Var x)
      OMS s -> return (Con (newSymbol (show s)))
      OMI n -> return (Num n)
      OMF a -> return (Float a)
      OMA (x:xs) -> liftM2 makeTerm (omobjToTerm x) (mapM omobjToTerm xs)
      OMBIND binder xs body ->
         omobjToTerm (OMA (binder:map OMV xs++[body]))
      _ -> fail "Invalid OpenMath object"
 where
   isMeta ('$':xs) = Just (foldl' (\a b -> a*10+ord b-48) 0 xs) -- '
   isMeta _        = Nothing

idToSymbol :: Id -> Symbol
idToSymbol a
   | null (qualifiers a) = 
        extraSymbol (unqualified a)
   | otherwise = 
        makeSymbol (qualification a) (unqualified a)

------------------------------------------------------------
-- Exercise Text data type
--    Note: ideally, this should be defined elsewhere

-- Exercise extension for textual feedback
data ExerciseText a = ExerciseText
   { ruleText              :: Rule (Context a) -> Maybe String
   , appliedRule           :: Rule (Context a) -> String
   , feedbackSyntaxError   :: String -> String
   , feedbackSame          :: String
   , feedbackBuggy         :: Bool -> [Rule (Context a)] -> String
   , feedbackNotEquivalent :: Bool -> String
   , feedbackOk            :: [Rule (Context a)] -> (String, Bool)
   , feedbackDetour        :: Bool -> Maybe (Rule (Context a)) -> [Rule (Context a)] -> (String, Bool)
   , feedbackUnknown       :: Bool -> String
   }