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
module Domain.Logic.FeedbackText (feedback, feedbackSyntaxError) where

import Common.Parsing
import Common.Transformation
import Service.TypedAbstractService
import Domain.Logic.Rules

-- Feedback messages for submit service (free student input). The boolean
-- indicates whether the student is allowed to continue (True), or forced 
-- to go back to the previous state (False)
feedback :: Result a -> (String, Bool)
feedback result = 
   case result of
      Buggy rs        -> (feedbackBuggy rs, False)
      NotEquivalent   -> (feedbackNotEquivalent, False)
      Ok rs _
         | null rs    -> (feedbackSame, False)
         | otherwise  -> feedbackOk rs
      Detour rs _     -> feedbackDetour rs
      Unknown _       -> (feedbackUnknown, False)

-- This is more general than the logic domain. Perhaps it should
-- be defined elsewhere
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

feedbackBuggy :: [Rule a] -> String
feedbackBuggy [br] 
   | br ~= buggyRuleCommImp = 
        incorrect "Did you think that implication is commutative? This is not the case. "
   | br ~= buggyRuleAssImp = 
        incorrect "Did you think that implication is associative? This is not the case. "
   | br ~= buggyRuleIdemImp = 
        incorrect "Did you think that implication is idempotent? This is not the case. "
   | br ~= buggyRuleIdemEqui = 
        incorrect "Did you think that equivalence is idempotent? This is not the case. "
   | br ~= buggyRuleAndSame = 
        incorrect "Did you think that phi AND phi is equivalent to True? This is not the case. Idempotency of AND means that phi AND phi is equivalent to phi. "
   | br ~= buggyRuleOrSame = 
        incorrect "Did you think that phi OR phi is equivalent to True? This is not the case. Idempotency of OR means that phi OR phi is equivalent to phi. "
   | br ~= buggyRuleAndCompl = 
        incorrect "Be careful in the application of the the complement-rules "
   | br ~= buggyRuleOrCompl = 
        incorrect "Be careful in the application of the the complement-rules " 
   | br ~= buggyRuleTrueProp = 
        incorrect "Be careful in the application of the the True-False rules "     
   | br ~= buggyRuleFalseProp = 
        incorrect "Be careful in the application of the the True-False rules " 
   | br ~= buggyRuleEquivElim1 = 
        incorrect "Be careful with the elimination of an equivalence; take care of the negations. "
   | br ~= buggyRuleEquivElim2 = 
        incorrect "Be careful with the elimination of an equivalence; did you interchange conjunctions and disjunctions? "
   | br ~= buggyRuleImplElim = 
        incorrect "Be careful with the elimination of an implication; make sure the negation is at the right place. "
   | br ~= buggyRuleDeMorgan1 = 
        incorrect "Did you try to apply DeMorgan? Be careful with the negations. " 
   | br ~= buggyRuleDeMorgan2 = 
        incorrect "Did you try to apply DeMorgan? Make sure that you remove the outer negation when applying this rule "
   | br ~= buggyRuleDeMorgan3 = 
        incorrect "Did you try to apply DeMorgan? Make sure that you replace AND by OR. "
   | br ~= buggyRuleDeMorgan4 = 
        incorrect "Did you try to apply DeMorgan? Make sure that you replace OR by AND. "
   | br ~= buggyRuleNotOverImpl = 
        incorrect "Did you think that you can distribute a negation over an implication? This is not the case. "
   | br ~= buggyRuleParenth1 = 
        incorrect "Take care of the negations and the parentheses" 
   | br ~= buggyRuleParenth2 = 
        incorrect "Take care of the outer negation when you eliminate an equivalence. " 
   | br ~= buggyRuleParenth3 = 
        incorrect "Did you try to apply double negation? At this place this is not allowed, because of the parenthesis between the negations. " 
   | br ~= buggyRuleAssoc = 
        incorrect "Did you change the negations? This is not allowed in a subformula consisting of a disjunction and a conjunction. " 
   | br ~= buggyRuleDistr = 
        incorrect "Did you try to apply distribution? Take care of the place of the disjunctions and the conjunctions. "  
               -- TODO Josje: aanvullen voor overige buggy regels [Gedaan!]
feedbackBuggy _ = incorrect ""

feedbackNotEquivalent :: String
feedbackNotEquivalent = incorrect ""
    
feedbackSame :: String
feedbackSame = "You have submitted the current term."

feedbackOk :: [Rule a] -> (String, Bool)
feedbackOk [one] = (okay (appliedRule one), True)
feedbackOk _     = ("You have combined multiple steps. Press the Back button and perform one step at the time.", False)

-- TODO Bastiaan: welke regel wordt er dan verwacht door de strategie?
feedbackDetour :: [Rule a] -> (String, Bool)
feedbackDetour [one] = (appliedRule one ++ " This is correct. However, the standard strategy suggests a different step.", True)
feedbackDetour _     = (feedbackUnknown, False)

feedbackUnknown :: String
feedbackUnknown = "You have combined multiple steps (or made a mistake). " ++ backAndHint 
    
appliedRule :: Rule a -> String
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

(~=) :: Rule a -> Rule b -> Bool
r1 ~= r2 = name r1 == name r2

-- TODO by Bastiaan
showToken :: Token -> String
showToken = show

-- TODO by Bastiaan
tokenPos :: Token -> String
tokenPos _ = "(??)"