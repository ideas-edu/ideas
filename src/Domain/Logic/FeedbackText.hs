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
module Domain.Logic.FeedbackText where

import Data.List
import Data.Maybe
import Common.Id
import Common.Transformation
import Domain.Logic.Rules
import Domain.Logic.BuggyRules

feedbackSyntaxError :: String -> String 
feedbackSyntaxError msg
   | take 1 msg == "("               = "Syntax error at " ++ msg
   | "Syntax error" `isPrefixOf` msg = msg
   | otherwise                       = "Syntax error: " ++ msg

feedbackBuggy :: Bool -> [Rule a] -> String
feedbackBuggy ready [br] 
   | br ~= buggyRuleCommImp = 
        f "Did you think that implication is commutative? This is not the case. "
   | br ~= buggyRuleAssImp = 
        f "Did you think that implication is associative? This is not the case. "
   | br ~= buggyRuleImplElim2 = 
        f "Make sure that you use the rule for implication elimanation, you seemed to use equivalence elimination "
   | br ~= buggyRuleEquivElim3 = 
        f "Make sure that you use the rule for equivalence elimanation, you seemed to use implication elimination "
   | br ~= buggyRuleIdemImp = 
        f "Did you think that implication is idempotent? This is not the case. "
   | br ~= buggyRuleIdemEqui = 
        f "Did you think that equivalence is idempotent? This is not the case. "
   | br ~= buggyRuleAndSame = 
        f "Did you think that phi AND phi is equivalent to True? This is not the case. Idempotency of AND means that phi AND phi is equivalent to phi. "
   | br ~= buggyRuleOrSame = 
        f "Did you think that phi OR phi is equivalent to True? This is not the case. Idempotency of OR means that phi OR phi is equivalent to phi. "
   | br ~= buggyRuleAndCompl = 
        f "Be careful in the application of the the complement-rules "
   | br ~= buggyRuleOrCompl = 
        f "Be careful in the application of the the complement-rules " 
   | br ~= buggyRuleTrueProp = 
        f "Be careful in the application of the the True-False rules "     
   | br ~= buggyRuleFalseProp = 
        f "Be careful in the application of the the True-False rules " 
   | br ~= buggyRuleEquivElim1 = 
        f "Be careful with the elimination of an equivalence; take care of the negations. "
   | br ~= buggyRuleEquivElim2 = 
        f "Be careful with the elimination of an equivalence; make sure that the disjunctions and the conjunctions are at the right place. "
   | br ~= buggyRuleImplElim = 
        f "Be careful with the elimination of an implication; make sure the negation is at the right place. "
   | br ~= buggyRuleImplElim1 = 
        f "Did you try to eliminate an implication? In that case you used an AND instead of an OR "
   | br ~= buggyRuleDeMorgan1 = 
        f "Did you try to apply DeMorgan? Be careful with the negations. " 
   | br ~= buggyRuleDeMorgan2 = 
        f "Did you try to apply DeMorgan? Make sure that you remove the outer negation when applying this rule "
   | br ~= buggyRuleDeMorgan3 = 
        f "Did you try to apply DeMorgan? Make sure that you replace AND by OR. "
   | br ~= buggyRuleDeMorgan4 = 
        f "Did you try to apply DeMorgan? Make sure that you replace OR by AND. "
   | br ~= buggyRuleDeMorgan5 = 
        f "Did you try to apply DeMorgan? Take care of the  scope of the negations. "        
   | br ~= buggyRuleNotOverImpl = 
        f "Did you think that you can distribute a negation over an implication? This is not the case. "
   | br ~= buggyRuleParenth1 = 
        f "Take care of the negations and the parentheses. " 
   | br ~= buggyRuleParenth2 = 
        f "Take care of the outer negation when you eliminate an equivalence. " 
   | br ~= buggyRuleParenth3 = 
        f "Did you try to apply double negation? At this place this is not allowed, because of the parenthesis between the negations. " 
   | br ~= buggyRuleAssoc = 
        f "Did you change the parentheses? This is not allowed in a subformula consisting of a disjunction and a conjunction. "
   | br ~= buggyRuleAbsor = 
        f "Did you try to apply absorption? You cant't apply this rule at this place since the resulting sub formula is not a subformula of the bigger term. "        
   | br ~= buggyRuleDistr = 
        f "Did you try to apply distribution? Take care of the place of the disjunctions and the conjunctions. "
   | br ~= buggyRuleDistrNot = 
        f "Did you try to apply distribution? Don't forget the negations!. "
 where f = incorrect ready
feedbackBuggy ready _ = incorrect ready ""

feedbackNotEquivalent :: Bool -> String
feedbackNotEquivalent ready = incorrect ready ""
    
feedbackSame :: String
feedbackSame = "You have submitted a similar term. " ++ 
   "Maybe you inserted or removed parentheses (the tool supports associativity)?"

feedbackOk :: [Rule a] -> (String, Bool)
feedbackOk [one] = (okay (appliedRule one), True)
feedbackOk _     = ("You have combined multiple steps. Press the Back button and perform one step at the time.", False)

-- TODO Bastiaan: welke regel wordt er dan verwacht door de strategie?
feedbackDetour :: Bool -> Maybe (Rule a) -> [Rule a] -> (String, Bool)
feedbackDetour True _ [one] = (appliedRule one ++ " " ++ feedbackFinished, True)
feedbackDetour True _ _     = (feedbackMultipleSteps ++ " " ++ feedbackFinished, True)
feedbackDetour _ _ [one] | one `inGroup` groupCommutativity =
   ("You have applied one of the commutativity rules correctly. This step is not mandatory, but sometimes helps to simplify the formula.", True)
feedbackDetour _ mexp [one] = 
   let however = case mexp >>= ruleText of
                    Just s  -> "However, the standard strategy suggests to use " ++ s ++ "." 
                    Nothing -> "However, the standard strategy suggests a different step."   
   in (appliedRule one ++ " This is correct. " ++ however, True)
feedbackDetour ready _ _ = (feedbackUnknown ready, False)

feedbackUnknown :: Bool -> String
feedbackUnknown ready = feedbackMultipleSteps ++ " " ++ backAndHint ready

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
   | r `inGroup` groupCommutativity = return "commutativity"
   | r `inGroup` groupAssociativity = return "associativity"
   | r `inGroup` groupDistributionOrOverAnd = return "distribution of or over and"
   | r `inGroup` groupDistributionAndOverOr = return "distribution of and over or"
   | r `inGroup` groupIdempotency = return "idempotency"
   | r `inGroup` groupAbsorption = return "absorption"
   | r `inGroup` groupDeMorgan = return "De Morgan"
   | r `inGroup` groupInverseDeMorgan = return "De Morgan"
   | r `inGroup` groupInverseDistr = return "distributivity"
    -- TODO Josje: aanvullen met alle regels (ook die ook in de DWA strategie voorkomen)
   | otherwise = Nothing
-------------------------------------------------------------------------
-- General text
  
incorrect :: Bool -> String -> String
incorrect ready s = "This is incorrect. " ++ s ++ backAndHint ready

okay :: String -> String
okay s = "Well done! " ++ s

backAndHint :: Bool -> String
backAndHint ready = "Press the Back button and try again." ++
   if ready then "" else " You may ask for a hint."

-------------------------------------------------------------------------
-- Helper functions

(~=) :: Rule a -> Rule b -> Bool
r1 ~= r2 = getId r1 == getId r2

-- Quick and dirty fix!
inGroup :: Rule a -> (Id, b) -> Bool
inGroup r n = 
   let rs = filter (~= r) (logicRules ++ buggyRules)
   in fst n `elem` concatMap ruleGroups rs