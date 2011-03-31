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
import Common.Utils (fst3)
import Data.Maybe
import Service.ExercisePackage
import Service.State
import Service.Diagnose
import Service.BasicServices
import Service.FeedbackScript
 
------------------------------------------------------------
-- Services

derivationtext :: State a -> Either String [(String, Context a)]
derivationtext state = do
   exText <- exerciseText state
   xs     <- derivation Nothing state
   return (map (first (toString (ready state) (script exText) . ruleText exText)) xs)

onefirsttext :: State a -> Maybe String -> Either String (Bool, String, State a)
onefirsttext state event =
   case onefirst state of
      Right (r, _, s) -> do
         exText <- exerciseText state
         let mtxt = fromContext (stateContext s) >>= useToRewrite exText r state
             msg  = case mtxt of
                       Just txt | event /= Just "hint button" -> txt
                       _ -> "Use " ++ toString (ready state) (script exText) (ruleText exText r)
         return (True, msg, s)
      Left _ -> return (False, "Sorry, no hint available", state)
      
submittext :: State a -> String -> Either String (Bool, String, State a)
submittext state input = do
   exText <- exerciseText state
   case parser (exercise (exercisePkg state)) input of
      Left err -> 
         return (False, toString (ready state) (script exText) $ feedbackSyntaxError exText err, state)
      Right a  -> 
         return (submitHelper exText state a)

-- Feedback messages for submit service (free student input). The boolean
-- indicates whether the student is allowed to continue (True), or forced 
-- to go back to the previous state (False)
submitHelper :: ExerciseText a -> State a -> a -> (Bool, String, State a)
submitHelper exText old a =
   case diagnose old a of
      Buggy r        -> ( False
                        , fromMaybe ""  (youRewroteInto old a) ++ 
                          toString oldReady (script exText) (feedbackBuggy exText r)
                        , old
                        )
      NotEquivalent  -> ( False
                        , fromMaybe ""  (youRewroteInto old a) ++
                          toString oldReady (script exText) (feedbackNotEquivalent exText)
                        , old
                        )
      Expected _ s r -> (True, toString oldReady (script exText) (feedbackOk exText r), s)
      Similar _ s    -> (True, toString oldReady (script exText) (feedbackSame exText), s)
      Detour _ s r   -> (True, toString oldReady (script exText) (feedbackDetour exText expected r), s)
      Correct _ s    -> ( False
                        , fromMaybe ""  (youRewroteInto old a) ++ 
                          toString oldReady (script exText) (feedbackUnknown exText)
                        , s
                        )
 where
   expected = either (const Nothing) (Just . fst3) (onefirst old)
   oldReady = ready old

------------------------------------------------------------
-- Helper functions

useToRewrite :: ExerciseText a -> Rule (Context a) -> State a -> a -> Maybe String
useToRewrite exText r old = rewriteIntoText True txt old
 where
   txt = "Use " ++ toString (ready old) (script exText) (ruleText exText r) ++ " to rewrite "

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