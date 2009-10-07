-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
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
module Domain.Logic.FeedbackText 
   ( feedbackSyntaxError, ruleText, appliedRule
   , feedbackBuggy, feedbackNotEquivalent, feedbackSame, feedbackOk, feedbackDetour, feedbackUnknown
   ) where

import Data.Maybe
import Text.Parsing
import Common.Transformation
import Domain.Logic.Rules
import Domain.Logic.BuggyRules

-- This is more general than the logic domain. Perhaps it should
-- be defined elsewhere
feedbackSyntaxError :: SyntaxError -> String
feedbackSyntaxError syntaxError =
   case syntaxError of
      ParNotClosed token -> 
         "Opening parenthesis symbol '(' at position " ++ tokenPos token ++ " is not closed."
      ParNoOpen token -> 
         "Closing parenthesis symbol ')' at position " ++ tokenPos token ++ " has no matching opening parenthesis."
      ParMismatch token1 token2 -> 
         "The openening parenthesis at position " ++ tokenPos token1 ++ 
         " does not match with the closing parenthesis at position " ++ tokenPos token2 ++ "."
      ErrorMessage txt -> 
         txt
      Unexpected token -> 
         "Unexpected " ++ showToken token

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
        incorrect "Be careful with the elimination of an equivalence; make sure that the disjunctions and the conjunctions are at the right place. "
   | br ~= buggyRuleImplElim = 
        incorrect "Be careful with the elimination of an implication; make sure the negation is at the right place. "
   | br ~= buggyRuleImplElim1 = 
        incorrect "Did you try to eliminate an implication? In that case you used an AND instead of an OR "
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
feedbackDetour :: Bool -> Maybe (Rule a) -> [Rule a] -> (String, Bool)
feedbackDetour True _ [one] = (appliedRule one ++ " " ++ feedbackFinished, True)
feedbackDetour True _ _     = (feedbackMultipleSteps ++ " " ++ feedbackFinished, True)
feedbackDetour _ _ [one] | one `inGroup`"Commutativity" =
   ("You have applied one of the commutativity rules correctly. This step is not mandatory, but sometimes helps to simplify the formula.", True)
feedbackDetour _ mexp [one] = 
   let however = case mexp >>= ruleText of
                    Just s  -> "However, the standard strategy suggests to use " ++ s ++ "." 
                    Nothing -> "However, the standard strategy suggests a different step."   
   in (appliedRule one ++ " This is correct. " ++ however, True)
feedbackDetour _ _ _ = (feedbackUnknown , False)

feedbackUnknown :: String
feedbackUnknown = feedbackMultipleSteps ++ " " ++ backAndHint 

feedbackMultipleSteps :: String
feedbackMultipleSteps = "You have combined multiple steps (or made a mistake)."

feedbackFinished :: String
feedbackFinished = "Are you aware that you already reached disjunctive normal form?"

appliedRule :: Rule a -> String
appliedRule r = "You have applied " ++ txt ++ " correctly."
 where txt = fromMaybe "some rule" (ruleText r)

ruleText :: Rule a -> Maybe String
ruleText r
   | r ~= ruleFalseZeroOr || r ~= ruleTrueZeroOr || r ~= ruleFalseZeroAnd || r ~= ruleTrueZeroAnd || r ~= ruleNotTrue || r ~= ruleNotFalse = 
        return "one of the False/True rules"
   | r ~= ruleComplOr || r ~= ruleComplAnd = return "a complement rule" 
   | r ~= ruleNotNot  = return "double negation" 
   | r ~= ruleDefImpl  = return "implication elimination" 
   | r ~= ruleDefEquiv  = return "equivalence elimination" 
   | r `inGroup`"Commutativity" = return "commutativity"
   | r `inGroup`"Aasociativity" = return "associativity"
   | r `inGroup`"Distributivity" = return "distributivity"
   | r `inGroup`"Idempotency" = return "idempotency"
   | r `inGroup`"Absorption" = return "absorption"
   | r `inGroup`"De Morgan" = return "De Morgan"
    -- TODO Josje: aanvullen met alle regels (ook die ook in de DWA strategie voorkomen)
   | otherwise = Nothing
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

-- Quick and dirty fix!
inGroup :: Rule a -> String -> Bool
inGroup r n = 
   let rs = filter (~= r) (logicRules ++ buggyRules)
   in n `elem` concatMap ruleGroups rs

showToken :: Token -> String
showToken token = tokenText token ++ " at position " ++ tokenPos token

tokenPos :: Token -> String
tokenPos token = 
   let p@(l, c) = toPosition token
   in if l==1 then show c else show p 
   