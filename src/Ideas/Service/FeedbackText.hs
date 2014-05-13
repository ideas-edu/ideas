{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- Copyright 2014, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Ideas.Service.FeedbackText
   ( Message, accept, text
   , onefirsttext, submittext, derivationtext, feedbacktext
   ) where

import Ideas.Common.Library hiding (derivation)
import Ideas.Service.BasicServices
import Ideas.Service.Diagnose
import Ideas.Service.FeedbackScript.Run
import Ideas.Service.FeedbackScript.Syntax
import Ideas.Service.State
import Ideas.Service.Types

data Message = M { accept :: Maybe Bool, text :: Text }

instance Typed a Message where
   typed = Tag "Message" $ Iso (f <-> g) typed
    where
      f   = either (\(b, t) -> M (Just b) t) (M Nothing)
      g m = maybe (Right (text m)) (\b -> Left (b, text m)) (accept m)

------------------------------------------------------------
-- Services

derivationtext :: Script -> State a -> Either String (Derivation String (Context a))
derivationtext script state =
   let f = ruleToString (newEnvironment state Nothing) script . fst
   in right (mapFirst f) (derivation Nothing state)

onefirsttext :: Script -> State a -> Maybe String -> (Message, Maybe (State a))
onefirsttext script old event =
   ( M Nothing (feedbackHint feedbackId env script)
   , fmap snd next
   )
 where
   feedbackId = newId $ if event == Just "hint button"
                        then "hint"
                        else "step"
   ex   = exercise old
   next = either (const Nothing) Just (onefirst old)
   env  = (newEnvironment old Nothing)
      { diffPair = do
          new      <- fmap snd next
          oldC     <- fromContext (stateContext old)
          a        <- fromContext (stateContext new)
          (d1, d2) <- difference ex oldC a
          return (prettyPrinter ex d1, prettyPrinter ex d2)
      }

-- Feedback messages for submit service (free student input). The boolean
-- indicates whether the student is allowed to continue (True), or forced
-- to go back to the previous state (False)
submittext :: Script -> State a -> String -> (Message, State a)
submittext script old input =
   case parser ex input of
      Left msg -> (M (Just False) (TextString msg), old)
      Right a  -> feedbacktext script old (inContext ex a) Nothing
 where
   ex = exercise old

feedbacktext :: Script -> State a -> Context a -> Maybe Id -> (Message, State a)
feedbacktext script old new motivationId =
   case diagnosis of
      Buggy _ _       -> (msg False, old)
      NotEquivalent _ -> (msg False, old)
      Expected _ s _  -> (msg True, s)
      WrongRule _ s _ -> (msg True, s)
      Similar _ s     -> (msg True, s)
      Detour _ s _ _  -> (msg True, s)
      Correct _ s     -> (msg False, s)
      Unknown _ s     -> (msg False, s)
 where
   diagnosis = diagnose old new motivationId
   output    = feedbackDiagnosis diagnosis env script
   msg b     = M (Just b) output
   ex        = exercise old
   motivationRule = motivationId >>= getRule ex
   env = (newEnvironment old motivationRule)
            { diffPair = do
                 oldTerm  <- fromContext (stateContext old)
                 newTerm  <- fromContext new
                 (d1, d2) <- difference ex oldTerm newTerm
                 return (prettyPrinter ex d1, prettyPrinter ex d2)
            }