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
import Domain.Logic.BuggyRules
import Service.FeedbackScript

script :: Script
script = buggyTable ++ ruleTable ++
   [ FeedbackSame (Text "You have submitted a similar term. \ 
        \Maybe you inserted or removed parentheses (the tool supports associativity)?")
   , FeedbackNotEq (incorrect <> backAndHint)
   , FeedbackUnknown (feedbackMultipleSteps <> backAndHint)
   , FeedbackOk (okay <> appliedRule)
   , FeedbackBuggy (incorrect <> AttrRecognized <> backAndHint)
   , FeedbackDetour detourText
   ]

buggyTable :: [Decl]
buggyTable = 
   [ f buggyRuleCommImp     "Did you think that implication is commutative? This is not the case. "
   , f buggyRuleAssImp      "Did you think that implication is associative? This is not the case. "
   , f buggyRuleImplElim2   "Make sure that you use the rule for implication elimanation, you seemed to use equivalence elimination "
   , f buggyRuleEquivElim3  "Make sure that you use the rule for equivalence elimanation, you seemed to use implication elimination "
   , f buggyRuleIdemImp     "Did you think that implication is idempotent? This is not the case. "
   , f buggyRuleIdemEqui    "Did you think that equivalence is idempotent? This is not the case. "
   , f buggyRuleAndSame     "Did you think that phi AND phi is equivalent to True? This is not the case. Idempotency of AND means that phi AND phi is equivalent to phi. "
   , f buggyRuleOrSame      "Did you think that phi OR phi is equivalent to True? This is not the case. Idempotency of OR means that phi OR phi is equivalent to phi. "
   , f buggyRuleAndCompl    "Be careful in the application of the the complement-rules "
   , f buggyRuleOrCompl     "Be careful in the application of the the complement-rules " 
   , f buggyRuleTrueProp    "Be careful in the application of the the True-False rules "     
   , f buggyRuleFalseProp   "Be careful in the application of the the True-False rules "
   , f buggyRuleEquivElim1  "Be careful with the elimination of an equivalence; take care of the negations. "
   , f buggyRuleEquivElim2  "Be careful with the elimination of an equivalence; make sure that the disjunctions and the conjunctions are at the right place. "
   , f buggyRuleImplElim    "Be careful with the elimination of an implication; make sure the negation is at the right place. "
   , f buggyRuleImplElim1   "Did you try to eliminate an implication? In that case you used an AND instead of an OR "
   , f buggyRuleDeMorgan1   "Did you try to apply DeMorgan? Be careful with the negations. "
   , f buggyRuleDeMorgan2   "Did you try to apply DeMorgan? Make sure that you remove the outer negation when applying this rule "
   , f buggyRuleDeMorgan3   "Did you try to apply DeMorgan? Make sure that you replace AND by OR. "
   , f buggyRuleDeMorgan4   "Did you try to apply DeMorgan? Make sure that you replace OR by AND. "
   , f buggyRuleDeMorgan5   "Did you try to apply DeMorgan? Take care of the  scope of the negations. "
   , f buggyRuleNotOverImpl "Did you think that you can distribute a negation over an implication? This is not the case. "
   , f buggyRuleParenth1    "Take care of the negations and the parentheses. "
   , f buggyRuleParenth2    "Take care of the outer negation when you eliminate an equivalence. "
   , f buggyRuleParenth3    "Did you try to apply double negation? At this place this is not allowed, because of the parenthesis between the negations. "
   , f buggyRuleAssoc       "Did you change the parentheses? This is not allowed in a subformula consisting of a disjunction and a conjunction. "
   , f buggyRuleAbsor       "Did you try to apply absorption? You cant't apply this rule at this place since the resulting sub formula is not a subformula of the bigger term. "
   , f buggyRuleDistr       "Did you try to apply distribution? Take care of the place of the disjunctions and the conjunctions. "
   , f buggyRuleDistrNot    "Did you try to apply distribution? Don't forget the negations! "
   ]
 where 
   f a b = RuleText (getId a) (Text b)

detourText :: Text
detourText = CondOldReady yes no
 where
   yes = appliedRule <> Text " " <> feedbackFinished
   no  = CondRecognizedIs (getId ruleCommOr) commText $
         CondRecognizedIs (getId ruleCommAnd) commText $
         appliedRule <> Text " This is correct. " <> however
         
   commText = Text "You have applied one of the commutativity rules correctly. This step is not mandatory, but sometimes helps to simplify the formula."
   however = CondHasExpected
                (Text "However, the standard strategy suggests to use " <> AttrExpected <> Text ".")
                (Text "However, the standard strategy suggests a different step.")

feedbackMultipleSteps :: Text
feedbackMultipleSteps = Text "You have combined multiple steps (or made a mistake). "

feedbackFinished :: Text
feedbackFinished = Text "Are you aware that you already reached disjunctive normal form?"

appliedRule :: Text
appliedRule = Text "You have applied " <> AttrRecognized <> Text " correctly."

ruleTable :: [Decl]
ruleTable = 
   [ f ruleFalseZeroOr  "one of the False/True rules"
   , f ruleTrueZeroOr   "one of the False/True rules"
   , f ruleFalseZeroAnd "one of the False/True rules"
   , f ruleTrueZeroAnd  "one of the False/True rules"
   , f ruleNotTrue      "one of the False/True rules"
   , f ruleNotFalse     "one of the False/True rules"
   , f ruleComplOr      "a complement rule" 
   , f ruleComplAnd     "a complement rule" 
   , f ruleNotNot       "double negation" 
   , f ruleDefImpl      "implication elimination" 
   , f ruleDefEquiv     "equivalence elimination" 
   ] ++ concat
   [ g groupCommutativity         "commutativity"
   , g groupAssociativity         "associativity"
   , g groupDistributionOrOverAnd "distribution of or over and"
   , g groupDistributionAndOverOr "distribution of and over or"
   , g groupIdempotency           "idempotency"
   , g groupAbsorption            "absorption"
   , g groupDeMorgan              "De Morgan"
   , g groupInverseDeMorgan       "De Morgan"
   , g groupInverseDistr          "distributivity"
   ]
 where
   f a b = RuleText (getId a) (Text b)
   g a b = [ f x b | x <- snd a ]

-------------------------------------------------------------------------
-- General text
  
incorrect :: Text
incorrect = Text "This is incorrect. " 

okay :: Text
okay = Text "Well done! "

backAndHint :: Text
backAndHint = Text "Press the Back button and try again." <>
   ifNotOldReady (Text " You may ask for a hint.")