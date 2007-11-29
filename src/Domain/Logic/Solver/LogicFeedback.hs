{--------------------------------------------------- 
Copyright (c)        2006 - 2007 
Johan Jeuring and Harrie Passier
---------------------------------------------------}

module Logic.Solver.LogicFeedback where

-- Standard Haskell libraries
import List
import Maybe (catMaybes, fromJust, isNothing)

-- UU libraries
import UU.Parsing
import UU.Pretty

-- Equations model      
import Logic.Solver.LogicDutchResources
import Logic.Solver.LogicPretty        
import Logic.Solver.LogicRewriteAnalysis
import Logic.Solver.LogicHint            
import Logic.Solver.LogicIndicator

import Logic 
import Logic.Parser
import Transformation


hasSolved newLogic oldLogic= 
  let parsedNewLogic    = fst $ parseLogic newLogic
      parsedOldLogic    = fst $ parseLogic oldLogic
      messages            = snd $ parseLogic newLogic
      feedbackstring      = disp (text (myShowMessages messages parsedNewLogic newLogic)) 40 ""
      indicatorsstring    = "" -- indicatorstext  parsedNewLogic (length (lines newLogic))  
  in if not (null messages) 
     then ( True, feedbackstring, indicatorsstring )
     else if solved parsedNewLogic parsedOldLogic    
          then (True, disp (text is_SolvedText)  40 "", indicatorsstring)
          else (True, disp (text not_solvedText) 40 "", indicatorsstring)

-- Detect whether a formulua is solved (in DNF-form and equal semantics)
-- f1 is the new formula, f2 the old formula.
solved :: Logic -> Logic -> Bool
solved f1 f2 = isDNF f2 && eqLogic f1 f2

feedback newLogic oldLogic =  
  let parsedNewLogic  = fst $ parseLogic newLogic
      feedbackstring    = disp (feedback' parsedNewLogic newLogic oldLogic) 40 ""
      indicatorsstring  = "" -- indicatorstext  parsedNewLogic (length (lines newLogic))
  in  if null feedbackstring 
      then  (False, feedbackstring, indicatorsstring) 
      else  (True,  feedbackstring, indicatorsstring) 


feedback' :: Logic -> String -> String -> PP_Doc
feedback' parsedNewLogic newLogic oldLogic 

  | not (null messages) = 
      {----------------------------------------------- 
      Scan and parse the submitted formula. 
      Give feedback when a parse-error is encountered, 
      and show the corrected formula.
      -----------------------------------------------}
      text (myShowMessages messages parsedNewLogic newLogic)

  | isDNF parsedOldLogic =
      {----------------------------------------------
       Check whether the formula was already solved in the
       previous step. Assumption is, that every formula in the history
       is correct.
      ------------------------------------------------}
       text formula_was_already_solved       

  |   (solved parsedNewLogic parsedOldLogic) && notAllowedRuleApplied (ruleInfo ruleApplied) && equalSolutions =
      {---------------------------------------------------- 
      The submitted formula is a correct solution,
      but an unknown rewrite rule or a sequence
      of rewrite rules is used.
      -----------------------------------------------------}
          text correctly_rewritten_butText
      >-< text unknown_rewrite_ruleText
      >-< text sequence_of_rewrite_rulesText
      
  
  |   eqLogic parsedNewLogic parsedOldLogic =  
      {----------------------------------------------- 
      Analyse if there has happend a correct rewritting,
      but an unknown rewrite rule or a sequence
      of rewrite rules have been used.
      -----------------------------------------------}
      case (ruleInfo ruleApplied) of
          NoRuleDetected       ->  text equivalent_butText
                               >-< text not_possible_to_rewriteText
                               >#< text into_this_formulaText
                               --   >#< dff
          otherwise            ->  text empty_stringText

  | not (eqLogic parsedNewLogic parsedOldLogic) =  
      {----------------------------------------------- 
      Calculate the solution of the formula. 
      Give feedback if the solution has changed compared
      with the previous solution. 
      ------------------------------------------------}
      case (ruleName fb) of
           NoDiff                   ->  text no_differenceText
                                        
           NoRuleDetected           ->  text you_have_rewrittenText
                                        >#<  form2string (removed fb)
                                        >#<  text in_Text
                                        >#<  {- prettyLogic-} form2string  (introduced fb)
                                        >|<  text dotText
                                        >-<  text this_rewritting_is_incorrectText
                                        >#<  text no_rule_detectedText
                                        >-<  text press_undoText
   
           TrueFalse                -> text you_have_tried_to_applyText
                                       >#<  printRule (ruleName fb)
                                       >#<  text on_the_partText
                                       >#<  {- prettyLogic-} form2string (removed fb)
                                       >-<  text incorrect_truefalseText

           CommutativityImplication ->  text you_have_tried_to_applyText
                                        >#<  printRule (ruleName fb)
                                        >#<  text on_the_partText
                                        >#<  {- prettyLogic-} form2string (removed fb)
                                        >-<  text however_implicationText
           
           RemoveDoubleNeg          ->  text incorrect_removal_of_negationsText
                                        -- >-<  text press_undoText          

           AssociativityImp         ->  text implication_is_not_associativeText
                                        -- >-<  text press_undoText         
           
           otherwise                ->  text incorrectly_appliedText
                                        >#<  printRule (ruleName fb)
                                        >|<  text dotText
                                        >-<  text you_have_rewrittenText
                                        >#<  {- prettyLogic -} form2string  (removed fb)
                                        >#<  text in_Text
                                        >#<  {- prettyLogic-} form2string (introduced fb)
                                        >|<  text dotText
                                        >-<  text correct_application_resultsText
                                        >#<  {- prettyLogic -} form2string  (correct fb)
                                        >|<  text dotText
                                        
    
  | otherwise = text empty_stringText
  
  where messages         =  snd $ parseLogic newLogic
        
        parsedOldLogic =  fst $ parseLogic oldLogic
  
        ruleApplied      =  allowedRuleApplied parsedOldLogic parsedNewLogic 

      
        fb               =  rewriteAnalyse parsedOldLogic parsedNewLogic
        
        printRule TrueFalse                = text truefalseText
        printRule EliminateImplication     = text elimination_implicationText 
        printRule EliminateEquivalence     = text elimination_equivalenceText
        printRule DeMorgan                 = text de_Morgan_ruleText
        printRule RemoveDoubleNeg          = text remove_double_negationsText
        printRule DistrAndoverOr           = text distribute_and_over_orText
        printRule Commutativity            = text commutativity_ruleText
        printRule CommutativityImplication = text commutativity_ruleText 
        printRule AssociativityImp         = text associativity_ruleText
        printRule AssociativityEqv         = text associativity_ruleText

        equalSolutions = eqLogic parsedNewLogic parsedOldLogic
      
        dff = if isNothing d
              then text no_diffText
              else     {- prettyLogic -} form2string (fst(fromJust d))
                   >#< text or_signText
                   >#< {- prettyLogic -} form2string (snd(fromJust d))
    
        d = diff (parsedOldLogic, parsedNewLogic)
{-------------------------------------------------------------------
  Function indicatortext displays the indicator information.
 -------------------------------------------------------------------} 

indicatorstext formula len = 
  let (i1, i2, i3) = indicators formula 
  in disp  (    text progressText
           >-<  indent 2 (text the_number_of_implicationsText)     
           >#<  indent 4 (pp i1)
           >-<  indent 2 (text the_number_of_equivalencesText)     
           >#<  indent 4 (pp i2)           
           >-<  indent 2 (text the_number_of_double_negationsText) 
           >#<  indent 4 (pp i3)
           )
           40 
           ""


{----------------------------------------------------------------------
 Function hint takes a formula and returns one hint.
 ---------------------------------------------------------------------}

hint             :: String -> String
hint  newLogic =  disp (hintpp (fst $ parseLogic newLogic)) 40 "" 

hintpp  ::  Logic -> PP_Doc
hintpp new  =
  case giveHint new of 
     Just (r, _)    -> text (rulepp $ name r)
     Nothing
        | isDNF new -> text you_have_already_solved_the_exerciseText  
	| otherwise -> text no_hint_availableText

{----------------------------------------------------------------------
 Function nextstep takes a formula and returns one next step.
 ---------------------------------------------------------------------}

nextstep             :: String -> String
nextstep  newLogic =  disp (nextstepp (fst $ parseLogic newLogic)) 40 "" 

nextstepp  ::  Logic -> PP_Doc
nextstepp new  =
  case giveHint new of 
     Just (r, p) ->
        text (rulepp $ name r)  -- !! TODO: maybe we have to change this text !!!!
        >#< form2string (noContext p)
        >#< text which_results_inText
        >#< form2string (noContext (applyD r p))
     Nothing
        | isDNF new ->
	     text you_have_already_solved_the_exerciseText  
        | otherwise ->
             text no_hint_availableText

rulepp :: String -> String
rulepp = id {-
   | rule==falseTrueRuleR = using_a_true_false_ruleText
   | rule==ruleDefImpl    = you_can_eliminate_the_implicationText
   | rule==ruleDefEquiv   = you_can_eliminate_the_equivalanceText
   | rule==ruleDeMorgan   = you_can_apply_De_Morgan_rule_onText
   | rule==ruleNotNot     = you_can_eliminate_the_double_negationText 
   | rule==ruleAndOverOr  = you_can_distribute_and_over_or_inText
   | otherwise            = name rule -}


