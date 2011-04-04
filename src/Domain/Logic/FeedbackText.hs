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

import Common.Id
import Domain.Logic.Rules
import Service.FeedbackScript
import Service.ScriptParser
import System.IO.Unsafe

pscript :: IO Script
pscript = parseScript "logic-script.txt"

script :: Script
script =
   unsafePerformIO pscript ++
   [ Feedback (newId "noteq") (youRewroteInto <> incorrect <> pressBack <> askForHint)
   , Feedback (newId "unknown") (youRewroteInto <> feedbackMultipleSteps <> pressBack <> askForHint)
   , Feedback (newId "buggy")  (youRewroteInto <> incorrect <> AttrRef (newId "recognized") <> pressBack <> askForHint)
   , Feedback (newId "detour") detourText
   ]

detourText :: Text
detourText = Conditional OldReady yes no
 where
   yes = appliedRule <> Text " " <> feedbackFinished
   no  = Conditional (RecognizedIs (getId ruleCommOr)) commText $
         Conditional (RecognizedIs (getId ruleCommAnd)) commText $
         appliedRule <> Text " This is correct. " <> however
         
   commText = Text "You have applied one of the commutativity rules correctly. This step is not mandatory, but sometimes helps to simplify the formula."
   however = Conditional HasExpected
                (Text "However, the standard strategy suggests to use " <> AttrRef (newId "expected") <> Text ".")
                (Text "However, the standard strategy suggests a different step.")

askForHint = Conditional OldReady Empty (Text " You may ask for a hint.")

appliedRule           = AttrRef $ newId "appliedRule"
incorrect             = AttrRef $ newId "incorrect"
okay                  = AttrRef $ newId "okay"
feedbackMultipleSteps = AttrRef $ newId "multiple"
feedbackFinished      = AttrRef $ newId "finished"
youRewroteInto        = AttrRef $ newId "youRewroteInto"
pressBack             = AttrRef $ newId "pressBack"