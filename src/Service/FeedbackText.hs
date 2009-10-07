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
module Service.FeedbackText 
   ( feedbackLogic
   , onefirsttext, submittext, derivationtext
   ) where

import Control.Arrow
import Common.Exercise
import Common.Utils (safeHead, fst3)
import Data.Maybe
import Domain.Logic.FeedbackText
import Domain.Logic.Exercises (dnfExercise)
import Service.TypedAbstractService
import Common.Context
import Common.Transformation (name, Rule)

-- Feedback messages for submit service (free student input). The boolean
-- indicates whether the student is allowed to continue (True), or forced 
-- to go back to the previous state (False)
feedbackLogic :: State a -> Result a -> (String, Bool)
feedbackLogic old result =
   case result of
      Buggy rs        -> (feedbackBuggy (ready old) rs, False)
      NotEquivalent   -> (feedbackNotEquivalent (ready old), False)
      Ok rs _
         | null rs    -> (feedbackSame, False)
         | otherwise  -> feedbackOk rs
      Detour rs _     -> feedbackDetour (ready old) (expected old) rs
      Unknown _       -> (feedbackUnknown (ready old), False)
 where
   expected = fmap fst3 . safeHead . allfirsts

showRule :: ExerciseCode -> Rule a -> String
showRule code r 
   | code == exerciseCode dnfExercise =
        fromMaybe txt (ruleText r)
   | otherwise = txt
 where
   txt = "rule " ++ name r

getCode :: State a -> ExerciseCode
getCode = exerciseCode . exercise

derivationtext :: State a -> [(String, Context a)]
derivationtext st = map (first (showRule (getCode st))) (derivation st)
   
onefirsttext :: State a -> (Bool, String, State a)
onefirsttext state =
   case allfirsts state of
      (r, _, s):_ -> (True, "Use " ++ showRule (getCode state) r, s)
      _ -> (False, "Sorry, no hint available", state)

submittext :: State a -> a -> (Bool, String, State a)
submittext state a = 
   case getResultState result of
      Just new | b -> (True, txt, resetStateIfNeeded new)
      _ -> (False, txt, state)
 where
  result = submit state a
  (txt, b) = feedbackLogic state result