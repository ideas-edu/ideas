-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
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
module Domain.Logic.FeedbackText (feedback) where

import Common.Parsing
import Common.Transformation
import Service.AbstractService
import Domain.Logic.Rules

-- Feedback messages for submit service (free student input). The boolean
-- indicates whether the student is allowed to continue (True), or forced 
-- to go back to the previous state (False)
feedback :: Result -> (String, Bool)
feedback result = 
   case result of
      SyntaxError err -> (feedbackSyntaxError err, False)
      Buggy rs        -> (feedbackBuggy rs, False)
      NotEquivalent   -> (feedbackNotEquivalent, False)
      Ok rs _
         | null rs    -> (feedbackSame, False)
         | otherwise  -> feedbackOk rs
      Detour rs _     -> feedbackDetour rs
      Unknown _       -> (feedbackUnknown, False)

feedbackSyntaxError :: SyntaxError -> String
feedbackSyntaxError syntaxError =
   case syntaxError of
      ParNotClosed token -> 
         "Opening parenthesis symbol ( at position " ++ tokenPos token ++ " is not closed."
      ParNoOpen token -> 
         "Closing parenthesis symbol ) at position " ++ tokenPos token ++ " has no matching opening parenthesis."
      ParMismatch token1 token2 -> 
         "The openening parenthesis at position " ++ tokenPos token1 ++ 
         " does not match with the closing parenthesis at position " ++ tokenPos token2 ++ "."
      ErrorMessage txt -> 
         txt
      Unexpected token -> 
         "Unexpected symbol " ++ showToken token

feedbackBuggy :: [RuleID] -> String
feedbackBuggy [one] 
   | one ~= buggyRuleCommImp = 
        incorrect "Did you think that implication is commutative? This is not the case. "
   | one ~= buggyRuleAssImp =
        incorrect "Did you think that implication is associative? This is not the case. "
   | one ~= buggyRuleIdemImp =
        incorrect "Did you think that implication is idempotent? This is not the case. "
   | one ~= buggyRuleIdemEqui =
        incorrect "Did you think that equivalence is idempotent? This is not the case. "
               -- TODO Josje: aanvullen voor overige buggy regels
feedbackBuggy _ = incorrect ""

feedbackNotEquivalent :: String
feedbackNotEquivalent = incorrect ""
    
feedbackSame :: String
feedbackSame = "You have submitted the current term."

feedbackOk :: [RuleID] -> (String, Bool)
feedbackOk [one] = (okay (appliedRule one), False)
feedbackOk _     = ("You have combined multiple steps. Press the Back button and perform one step at the time.", False)

-- TODO Bastiaan: welke regel wordt er dan verwacht door de strategie?
feedbackDetour :: [RuleID] -> (String, Bool)
feedbackDetour [one] = (appliedRule one ++ " This is correct. However, the standard strategy suggests a different step.", True)
feedbackDetour _     = (feedbackUnknown, False)

feedbackUnknown :: String
feedbackUnknown = "You have combined multiple steps (or made a mistake). " ++ backAndHint 
    
appliedRule :: RuleID -> String
appliedRule r = "You have applied " ++ txt ++ "."
 where
   txt | r ~= ruleFalseZeroOr || r ~= ruleTrueZeroOr = "one of the False/True rules"
       | otherwise = " a rule correctly"
    -- TODO Josje: aanvullen met alle regels (ook die ook in de DWA strategie voorkomen)
       
-------------------------------------------------------------------------
-- General text
  
incorrect :: String -> String
incorrect s = "This is incorrect. " ++ s ++ backAndHint

okay :: String -> String
okay s = "Well done! " ++ s

backAndHint :: String
backAndHint = "Press the Back button and try again. You may ask for a hint."

-------------------------------------------------------------------------
-- Helper functions

(~=) :: RuleID -> Rule a -> Bool
rid ~= r = name r == rid

-- TODO by Bastiaan
showToken :: Token -> String
showToken = show

-- TODO by Bastiaan
tokenPos :: Token -> String
tokenPos _ = "(??)"