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
import Service.ScriptParser

feedbacktext :: FilePath -> State a -> a -> IO String
feedbacktext file st a = do
   script <- parseScript file
   return (runScript (diagnose st a) script)

runScript :: Diagnosis a -> Script -> String
runScript diagnosis script = 
   case diagnosis of
      Buggy r         -> r ? script
      NotEquivalent   -> newId "noteq" ? script
      Missing         -> newId "noteq" ? script
      IncorrectPart _ -> newId "noteq" ? script
      Similar _ _     -> newId "similar" ? script
      Expected _ _ r  -> r ? script
      Detour _ _ r    -> r ? script
      Correct _ _     -> newId "correct" ? script
   
(?) :: HasId a => a -> Script -> String
a ? xs = case lookup (getId a) xs of
            Just s  -> s
            Nothing -> "No feedback for " ++ showId a