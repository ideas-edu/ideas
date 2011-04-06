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
   ( onefirsttext, submittext, derivationtext, feedbacktext, newRuleText
   ) where

import Common.Library hiding (derivation)
import Common.Utils (fst3, thd3)
import Control.Monad
import Data.Maybe
import Service.ExercisePackage
import Service.State
import Service.Diagnose
import Service.BasicServices
import Service.FeedbackScript
import Service.ScriptParser

newRuleText :: Script -> Rule a -> String -- TODO: remove me
newRuleText script r = 
   toString emptyEnvironment script [TextRef (getId r)]

------------------------------------------------------------
-- Services

derivationtext :: State a -> Either String [(String, Context a)]
derivationtext state = do
   script <- exerciseScript state
   xs     <- derivation Nothing state
   return (map (first (newRuleText script)) xs)

onefirsttext :: State a -> Maybe String -> Either String (Bool, String, State a)
onefirsttext old event = do
   script <- exerciseScript old
   return ( isJust next
          , feedbackHint (event == Just "hint button") env script
          , maybe old thd3 next
          )
 where
   ex   = exercise (exercisePkg old) 
   next = either (const Nothing) Just (onefirst old)
   env  = emptyEnvironment
      { oldReady = Just (ready old)
      , expected = fmap fst3 next
      , diffPair = do
          new      <- fmap thd3 next
          oldC     <- fromContext (stateContext old)
          a        <- fromContext (stateContext new)
          (d1, d2) <- difference ex True oldC a 
          return (prettyPrinter ex d1, prettyPrinter ex d2)
      }

-- Feedback messages for submit service (free student input). The boolean
-- indicates whether the student is allowed to continue (True), or forced 
-- to go back to the previous state (False)      
submittext :: State a -> String -> Either String (Bool, String, State a)
submittext old input = do
   script <- exerciseScript old
   return $
      case parser (exercise (exercisePkg old)) input of
         Left msg -> (False, msg, old)
         Right a  -> (feedbackWith old a script)

feedbacktext :: FilePath -> State a -> a -> IO (Bool, String, State a)
feedbacktext file old a =
   liftM (feedbackWith old a) (parseScript file)

feedbackWith :: State a -> a -> Script -> (Bool, String, State a)
feedbackWith old a script =
   case diagnose old a of
      Buggy r        -> (False, feedbackBuggy env {recognized = Just r} script, old)
      NotEquivalent  -> (False, feedbackNotEq env script, old)
      Expected _ s r -> (True, feedbackOk env {recognized = Just r} script, s)
      Similar _ s    -> (True, feedbackSame env script, s)
      Detour _ s r   -> (True, feedbackDetour env {recognized = Just r} script, s)
      Correct _ s    -> (False, feedbackUnknown env script, s)
 where
   ex  = exercise (exercisePkg old)
   env = emptyEnvironment 
            { oldReady = Just (ready old)
            , expected = either (const Nothing) (Just . fst3) (onefirst old)
            , diffPair = do
                 oldC     <- fromContext (stateContext old)
                 (d1, d2) <- difference ex False oldC a 
                 return (prettyPrinter ex d1, prettyPrinter ex d2)
            }

------------------------------------------------------------
-- Helper function

exerciseScript :: State a -> Either String Script
exerciseScript = 
   let msg = "No support for textual feedback"
   in maybe (fail msg) return . getScript . exercisePkg