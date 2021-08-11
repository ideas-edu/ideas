{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- Copyright 2019, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------

module Ideas.Service.FeedbackText
   ( Message, tMessage, accept, text
   , onefirsttext, submittext, derivationtext, feedbacktext
   ) where

import Ideas.Common.Library
import Ideas.Service.BasicServices
import Ideas.Service.Diagnose
import Ideas.Service.FeedbackScript.Run
import Ideas.Service.FeedbackScript.Syntax
import Ideas.Service.State
import Ideas.Service.Types
import Ideas.Utils.Prelude

data Message = M { accept :: Maybe Bool, text :: Text }

tMessage :: Type a Message
tMessage = Iso (f <-> g) tp
 where
   tp  = tPair (Tag "accept" tBool) (Tag "message" tText) :|: (Tag "message" tText)
   f   = either (\(b, t) -> M (Just b) t) (M Nothing)
   g m = maybe (Right (text m)) (\b -> Left (b, text m)) (accept m)

------------------------------------------------------------
-- Services

derivationtext :: Script -> State a -> Either String (Derivation String (Context a))
derivationtext script state =
   let f = ruleToString (newEnvironment state Nothing) script . fst3
   in right (mapFirst f) (solution Nothing state)

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
submittext script old txt =
   case parser ex txt of
      Left msg -> (M (Just False) (TextString msg), old)
      Right a  -> feedbacktext script old (inContext ex a) Nothing
 where
   ex = exercise old

feedbacktext :: Script -> State a -> Context a -> Maybe Id -> (Message, State a)
feedbacktext script old new motivationId =
   case diagnosis of
      SyntaxError s   -> (M (Just False) (makeText s), old)
      Buggy _ _       -> (msg False, old)
      NotEquivalent _ -> (msg False, old)
      Expected _ s _  -> (msg True, s)
      WrongRule _ s _ -> (msg True, s)
      Similar _ s _   -> (msg True, s)
      Detour _ s _ _  -> (msg True, s)
      Correct _ s     -> (msg False, s)
      Unknown _ s     -> (msg False, s)
 where
   diagnosis = diagnose old new motivationId
   out       = feedbackDiagnosis diagnosis env script
   msg b     = M (Just b) out
   ex        = exercise old
   motivationRule = motivationId >>= getRule ex
   env = (newEnvironment old motivationRule)
            { diffPair = do
                 oldTerm  <- fromContext (stateContext old)
                 newTerm  <- fromContext new
                 (d1, d2) <- difference ex oldTerm newTerm
                 return (prettyPrinter ex d1, prettyPrinter ex d2)
            }