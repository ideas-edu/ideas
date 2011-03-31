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
-- Execute script with diagnosis
--
-----------------------------------------------------------------------------
module Service.RunScript (feedbacktext) where

import Common.Id
import Service.Diagnose
import Service.State
import Service.FeedbackScript
import Service.ScriptParser
import Service.BasicServices (ready)

feedbacktext :: FilePath -> State a -> a -> IO (Bool, String, State a)
feedbacktext file oldState a = do
   script <- parseScript file
   let find x   = toString oldReady script (x ? script)
       oldReady = ready oldState
   return $ 
      case diagnose oldState a of
         Buggy r         -> (False, find r, oldState)
         NotEquivalent   -> (False, find $ newId "noteq", oldState)
         Similar _ st    -> (True, find $ newId "similar", st)
         Expected _ st r -> (True, find r, st)
         Detour _ st r   -> (True, find r, st)
         Correct _ st    -> (True, find $ newId "correct", st)
   
(?) :: HasId a => a -> Script -> Text
a ? xs = case [ t | RuleText b t <- xs, getId a==b ] of
            s:_ -> s
            []  -> Text ("No feedback for " ++ showId a)