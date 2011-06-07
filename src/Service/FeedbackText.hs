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
   ( onefirsttext, submittext, derivationtext, feedbacktext
   ) where

import Common.Library hiding (derivation)
import Service.State
import Service.Diagnose
import Service.BasicServices
import Service.FeedbackScript.Run
import Service.FeedbackScript.Syntax

------------------------------------------------------------
-- Services

derivationtext :: Script -> State a -> Either String (Derivation String (Context a))
derivationtext script state =
   let f = ruleToString (newEnvironment state) script . fst
   in right (mapFirst f) (derivation Nothing state)

onefirsttext :: Script -> State a -> Maybe String -> (Text, Maybe (State a))
onefirsttext script old event = 
   ( feedbackHint (event == Just "hint button") env script
   , fmap fth4 next
   )
 where
   ex   = exercise old
   next = either (const Nothing) Just (onefirst old)
   fth4 (_, _, _, a) = a
   env  = (newEnvironment old)
      { diffPair = do
          new      <- fmap fth4 next
          oldC     <- fromContext (stateContext old)
          a        <- fromContext (stateContext new)
          (d1, d2) <- difference ex False oldC a 
          return (prettyPrinter ex d1, prettyPrinter ex d2)
      }

-- Feedback messages for submit service (free student input). The boolean
-- indicates whether the student is allowed to continue (True), or forced 
-- to go back to the previous state (False)      
submittext :: Script -> State a -> String -> (Bool, Text, State a)
submittext script old input =
   case parser (exercise old) input of
      Left msg -> (False, TextString msg, old)
      Right a  -> feedbacktext script old a

feedbacktext :: Script -> State a -> a -> (Bool, Text, State a)
feedbacktext script old a =
   case diagnosis of
      Buggy _        -> (False, output, old)
      NotEquivalent  -> (False, output, old)
      Expected _ s _ -> (True,  output, s)
      Similar _ s    -> (True,  output, s)
      Detour _ s _   -> (True,  output, s)
      Correct _ s    -> (False, output, s)
 where
   diagnosis = diagnose old a
   output    = feedbackDiagnosis diagnosis env script
   ex  = exercise old
   env = (newEnvironment old)
            { diffPair = do
                 oldC     <- fromContext (stateContext old)
                 (d1, d2) <- difference ex False oldC a 
                 return (prettyPrinter ex d1, prettyPrinter ex d2)
            }
