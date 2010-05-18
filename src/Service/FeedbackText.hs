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

import Control.Arrow
import Common.Context
import Common.Exercise
import Common.Transformation
import Data.Maybe
import Common.Utils
import Service.Diagnose (restartIfNeeded)
import Service.ExercisePackage
import Service.State
import Service.Submit
import Service.BasicServices
 
------------------------------------------------------------
-- Services

derivationtext :: Monad m => State a -> Maybe String -> m [(String, Context a)]
derivationtext state _event = do
   exText <- exerciseText state
   xs     <- derivation Nothing state
   return (map (first (showRule exText)) xs)

onefirsttext :: Monad m => State a -> Maybe String -> m (Bool, String, State a)
onefirsttext state event =
   case onefirst state of
      Just (r, _, s) -> do
         exText <- exerciseText state
         let mtxt = fromContext (context s) >>= useToRewrite exText r state
             msg  = case mtxt of
                       Just txt | event /= Just "hint button" -> txt
                       _ -> "Use " ++ showRule exText r
         return (True, msg, s)
      _ -> return (False, "Sorry, no hint available", state)
      
submittext :: Monad m => State a -> String -> Maybe String -> m (Bool, String, State a)
submittext state txt _event = do
   exText <- exerciseText state
   return $
      case parser (exercise (exercisePkg state)) txt of
         Left err -> 
            (False, feedbackSyntaxError exText err, state)
         Right a  -> 
            let result = submit state a
                (txt, b) = submitHelper exText state a result
            in case getResultState result of
                  Just new | b -> (True, txt, restartIfNeeded new)
                  _ -> (False, txt, state)

-- Feedback messages for submit service (free student input). The boolean
-- indicates whether the student is allowed to continue (True), or forced 
-- to go back to the previous state (False)
submitHelper :: ExerciseText a -> State a -> a -> Result a -> (String, Bool)
submitHelper exText old a result =
   case result of
      Buggy rs        -> ( fromMaybe ""  (youRewroteInto old a) ++ 
                           feedbackBuggy exText (ready old) rs
                         , False)
      NotEquivalent   -> ( fromMaybe ""  (youRewroteInto old a) ++
                           feedbackNotEquivalent exText (ready old)
                         , False)
      Ok rs _
         | null rs    -> (feedbackSame exText, False)
         | otherwise  -> feedbackOk exText rs
      Detour rs _     -> feedbackDetour exText (ready old) (expected old) rs
      Unknown _       -> ( fromMaybe ""  (youRewroteInto old a) ++ 
                           feedbackUnknown exText (ready old)
                         , False)
 where
   expected s = do
      xs <- allfirsts s
      fmap fst3 (safeHead xs)

------------------------------------------------------------
-- Helper functions

showRule :: ExerciseText a -> Rule (Context a) -> String
showRule exText r = 
   case ruleText exText r of
      Just s  -> s
      Nothing -> "rule " ++ name r

useToRewrite :: ExerciseText a -> Rule (Context a) -> State a -> a -> Maybe String
useToRewrite exText rule old = rewriteIntoText True txt old
 where
   txt = "Use " ++ showRule exText rule
         ++ " to rewrite "

youRewroteInto :: State a -> a -> Maybe String
youRewroteInto = rewriteIntoText False "You rewrote "

rewriteIntoText :: Bool -> String -> State a -> a -> Maybe String
rewriteIntoText mode txt old a = do
   let ex = exercise (exercisePkg old)
   p <- fromContext (context old)
   (p1, a1) <- difference ex mode p a 
   return $ txt ++ prettyPrinter ex p1 
         ++ " into " ++ prettyPrinter ex a1 ++ ". "

exerciseText :: Monad m => State a -> m (ExerciseText a)
exerciseText = 
   let msg = "No support for textual feedback"
   in maybe (fail msg) return . getExerciseText . exercisePkg