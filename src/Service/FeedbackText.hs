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
module Service.FeedbackText 
   ( ExerciseText(..)
   , onefirsttext, submittext, derivationtext, submitHelper
   ) where

import Common.Library hiding (derivation)
import Data.Maybe
import Service.ExercisePackage
import Service.State
import Service.Diagnose
import Service.BasicServices
 
------------------------------------------------------------
-- Services

derivationtext :: State a -> Either String [(String, Context a)]
derivationtext state = do
   exText <- exerciseText state
   xs     <- derivation Nothing state
   return (map (first (showRule exText)) xs)

onefirsttext :: State a -> Maybe String -> Either String (Bool, String, State a)
onefirsttext state event =
   case onefirst state of
      Right (r, _, s) -> do
         exText <- exerciseText state
         let mtxt = fromContext (stateContext s) >>= useToRewrite exText r state
             msg  = case mtxt of
                       Just txt | event /= Just "hint button" -> txt
                       _ -> "Use " ++ showRule exText r
         return (True, msg, s)
      Left _ -> return (False, "Sorry, no hint available", state)
      
submittext :: State a -> String -> Either String (Bool, String, State a)
submittext state input = do
   exText <- exerciseText state
   return $
      case parser (exercise (exercisePkg state)) input of
         Left err -> 
            (False, feedbackSyntaxError exText err, state)
         Right a  -> 
            let result = diagnose state a
                (b, txt) = submitHelper exText state a result
            in case diagnosisState result of
                  Just new | b -> (True, txt, restartIfNeeded new)
                  _ -> (False, txt, state)

-- Feedback messages for submit service (free student input). The boolean
-- indicates whether the student is allowed to continue (True), or forced 
-- to go back to the previous state (False)
submitHelper :: ExerciseText a -> State a -> a -> Diagnosis a -> (Bool, String)
submitHelper exText old a result =
   case result of
      Buggy r        -> ( False
                        , fromMaybe ""  (youRewroteInto old a) ++ 
                          feedbackBuggy exText (ready old) r
                        )
      NotEquivalent  -> ( False
                        , fromMaybe ""  (youRewroteInto old a) ++
                          feedbackNotEquivalent exText (ready old)
                        )
      Expected _ _ r -> (True, feedbackOk exText r)
      Similar _ _    -> (False, feedbackSame exText)
      Detour _ _ r   -> (True, feedbackDetour exText (ready old) (expected old) r)
      Correct _ _    -> ( False
                        , fromMaybe ""  (youRewroteInto old a) ++ 
                          feedbackUnknown exText (ready old)
                        )
 where
   expected s = 
      case onefirst s of
         Right (r, _, _) -> Just r
         _               -> Nothing

------------------------------------------------------------
-- Helper functions

showRule :: ExerciseText a -> Rule (Context a) -> String
showRule exText r = 
   fromMaybe ("rule " ++ showId r) (ruleText exText r)

useToRewrite :: ExerciseText a -> Rule (Context a) -> State a -> a -> Maybe String
useToRewrite exText r old = rewriteIntoText True txt old
 where
   txt = "Use " ++ showRule exText r ++ " to rewrite "

youRewroteInto :: State a -> a -> Maybe String
youRewroteInto = rewriteIntoText False "You rewrote "

rewriteIntoText :: Bool -> String -> State a -> a -> Maybe String
rewriteIntoText mode txt old a = do
   let ex = exercise (exercisePkg old)
   p <- fromContext (stateContext old)
   (p1, a1) <- difference ex mode p a 
   return $ txt ++ prettyPrinter ex p1 
         ++ " into " ++ prettyPrinter ex a1 ++ ". "

exerciseText :: State a -> Either String (ExerciseText a)
exerciseText = 
   let msg = "No support for textual feedback"
   in maybe (fail msg) return . getExerciseText . exercisePkg