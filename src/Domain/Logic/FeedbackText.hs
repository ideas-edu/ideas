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
-- Feedback messages reported for the logic domain. Content to be provided 
-- by Josje Lodder.
--
-----------------------------------------------------------------------------
module Domain.Logic.FeedbackText (script) where

import Service.FeedbackScript
import Service.ScriptParser
import System.IO.Unsafe

scriptIO :: IO Script
scriptIO = parseScript "scripts/logic.txt" -- `catch` const (return [])

script :: Script
script =
   unsafePerformIO scriptIO 
