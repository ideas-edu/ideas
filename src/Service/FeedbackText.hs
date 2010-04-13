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
import Common.Utils
import Data.Maybe
import Service.Diagnose (restartIfNeeded)
import Service.Submit
import Service.TypedAbstractService
import Text.Parsing (SyntaxError, errorPositions)

------------------------------------------------------------
-- Exercise Text data type

-- Exercise extension for textual feedback
data ExerciseText a = ExerciseText
   { ruleText              :: Rule (Context a) -> Maybe String
   , appliedRule           :: Rule (Context a) -> String
   , feedbackSyntaxError   :: SyntaxError -> String
   , feedbackSame          :: String
   , feedbackBuggy         :: Bool -> [Rule (Context a)] -> String
   , feedbackNotEquivalent :: Bool -> String
   , feedbackOk            :: [Rule (Context a)] -> (String, Bool)
   , feedbackDetour        :: Bool -> Maybe (Rule (Context a)) -> [Rule (Context a)] -> (String, Bool)
   , feedbackUnknown       :: Bool -> String
   }

------------------------------------------------------------
-- Services

derivationtext :: Monad m => ExerciseText a -> State a -> Maybe String -> m [(String, Context a)]
derivationtext exText st _event = do
   xs <- derivation Nothing st
   return (map (first (showRule exText)) xs)

onefirsttext :: ExerciseText a -> State a -> Maybe String -> (Bool, String, State a)
onefirsttext exText state event =
   case allfirsts state of
      Just ((r, _, s):_) ->
         let msg = case fromContext (context s) >>= useToRewrite exText r state of
                      Just txt | event /= Just "hint button" -> txt
                      _ -> "Use " ++ showRule exText r
         in (True, msg, s)
      _ -> (False, "Sorry, no hint available", state)
      
submittext :: ExerciseText a -> State a -> String -> Maybe String -> (Bool, String, State a)
submittext exText state txt _event = 
   case parser (exercise state) txt of
      Left err -> 
         let msg = "Syntax error" ++ pos ++ ": " ++ show err
             pos = case map show (errorPositions err) of
                      [] -> ""
                      xs -> " at " ++ commaList xs
         in (False, msg, state)
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
   let ex = exercise old
   p <- fromContext (context old)
   (p1, a1) <- difference ex mode p a 
   return $ txt ++ prettyPrinter ex p1 
         ++ " into " ++ prettyPrinter ex a1 ++ ". "