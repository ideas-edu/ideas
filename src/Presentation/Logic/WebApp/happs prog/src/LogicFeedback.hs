{--------------------------------------------------- 
Copyright (c)        2006 - 2007 
Johan Jeuring and Harrie Passier
---------------------------------------------------}

module LogicFeedback where

-- Standard Haskell libraries
import List
import Maybe (catMaybes, fromJust, isNothing)

-- UU libraries
import UU.Parsing
import UU.Pretty

-- Equations parser
import LogicParser -- (myShowMessages)

-- Equations model
import LogicFormula         
-- import LogicEnglishResources
import LogicDutchResources
import LogicParser          
import LogicPretty        
import LogicRewriteAnalysis
import LogicHint            
import LogicIndicator


hasSolved newFormula oldFormula= 
  let parsedNewFormula    = parseFormula newFormula
      parsedOldFormula    = parseFormula oldFormula
      messages            = parseMsgs newFormula
      feedbackstring      = disp (text (myShowMessages messages parsedNewFormula newFormula)) 40 ""
      indicatorsstring    = "" -- indicatorstext  parsedNewFormula (length (lines newFormula))  
  in if not (null messages) 
     then ( True, feedbackstring, indicatorsstring )
     else if solved parsedNewFormula parsedOldFormula    
          then (True, disp (text is_SolvedText)  40 "", indicatorsstring)
          else (True, disp (text not_solvedText) 40 "", indicatorsstring)


feedback newFormula oldFormula =  
  let parsedNewFormula  = parseFormula newFormula
      feedbackstring    = disp (feedback' parsedNewFormula newFormula oldFormula) 40 ""
      indicatorsstring  = "" -- indicatorstext  parsedNewFormula (length (lines newFormula))
  in  if null feedbackstring 
      then  (False, feedbackstring, indicatorsstring) 
      else  (True,  feedbackstring, indicatorsstring) 


feedback' :: Formula -> String -> String -> PP_Doc
feedback' parsedNewFormula newFormula oldFormula 

  | not (null messages) = 
      {----------------------------------------------- 
      Scan and parse the submitted formula. 
      Give feedback when a parse-error is encountered, 
      and show the corrected formula.
      -----------------------------------------------}
      text (myShowMessages messages parsedNewFormula newFormula)

  | isDnf parsedOldFormula =
      {----------------------------------------------
       Check whether the formula was already solved in the
       previous step. Assumption is, that every formula in the history
       is correct.
      ------------------------------------------------}
       text formula_was_already_solved       

  |   (solved parsedNewFormula parsedOldFormula) && notAllowedRuleApplied (ruleInfo ruleApplied) && equalSolutions =
      {---------------------------------------------------- 
      The submitted formula is a correct solution,
      but an unknown rewrite rule or a sequence
      of rewrite rules is used.
      -----------------------------------------------------}
          text correctly_rewritten_butText
      >-< text unknown_rewrite_ruleText
      >-< text sequence_of_rewrite_rulesText
      
  
  |   equal parsedNewFormula parsedOldFormula =  
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

  | not (equal parsedNewFormula parsedOldFormula) =  
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
                                        >#<  {- prettyFormula-} form2string  (introduced fb)
                                        >|<  text dotText
                                        >-<  text this_rewritting_is_incorrectText
                                        >#<  text no_rule_detectedText
                                        >-<  text press_undoText
   
           TrueFalse                -> text you_have_tried_to_applyText
                                       >#<  printRule (ruleName fb)
                                       >#<  text on_the_partText
                                       >#<  {- prettyFormula-} form2string (removed fb)
                                       >-<  text incorrect_truefalseText

           CommutativityImplication ->  text you_have_tried_to_applyText
                                        >#<  printRule (ruleName fb)
                                        >#<  text on_the_partText
                                        >#<  {- prettyFormula-} form2string (removed fb)
                                        >-<  text however_implicationText
           
           RemoveDoubleNeg          ->  text incorrect_removal_of_negationsText
                                        -- >-<  text press_undoText          

           AssociativityImp         ->  text implication_is_not_associativeText
                                        -- >-<  text press_undoText         
           
           otherwise                ->  text incorrectly_appliedText
                                        >#<  printRule (ruleName fb)
                                        >|<  text dotText
                                        >-<  text you_have_rewrittenText
                                        >#<  {- prettyFormula -} form2string  (removed fb)
                                        >#<  text in_Text
                                        >#<  {- prettyFormula-} form2string (introduced fb)
                                        >|<  text dotText
                                        >-<  text correct_application_resultsText
                                        >#<  {- prettyFormula -} form2string  (correct fb)
                                        >|<  text dotText
                                        
    
  | otherwise = text empty_stringText
  
  where messages         =  parseMsgs newFormula
        
        parsedOldFormula =  parseFormula oldFormula
  
        ruleApplied      =  allowedRuleApplied parsedOldFormula parsedNewFormula 

      
        fb               =  rewriteAnalyse parsedOldFormula parsedNewFormula
        
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

        equalSolutions = equal parsedNewFormula parsedOldFormula
      
        dff = if isNothing d
              then text no_diffText
              else     {- prettyFormula -} form2string (fst(fromJust d))
                   >#< text or_signText
                   >#< {- prettyFormula -} form2string (snd(fromJust d))
    
        d = diff (parsedOldFormula, parsedNewFormula)
{-------------------------------------------------------------------
  Function indicatortext displays the indicator information.
 -------------------------------------------------------------------} 

indicatorstext formula len = 
  let ind = indicators formula 
  in disp  (    text progressText
           >-<  indent 2 (text the_number_of_implicationsText)     
           >#<  indent 4 (pp (numOfImp ind))
           >-<  indent 2 (text the_number_of_equivalencesText)     
           >#<  indent 4 (pp (numOfEqv ind))           
           >-<  indent 2 (text the_number_of_double_negationsText) 
           >#<  indent 4 (pp (numOfDoubleNeg ind))
           )
           40 
           ""


{----------------------------------------------------------------------
 Function hint takes a formula and returns one hint.
 ---------------------------------------------------------------------}

hint             :: String -> String
hint  newFormula =  disp (hintpp (parseFormula newFormula)) 40 "" 


hintpp  ::  Formula -> PP_Doc
hintpp  new  =

  case giveHint new of 
   
    (TrueFalseHint True f1 f2           ) ->  text you_can_simplify_the_formula_using_true_false_ruleText    

    (EliminateImplicationHint True f1 f2) ->  text you_can_eliminate_an_implicationText
                                                                                        
    (EliminateEquivalenceHint True f1 f2) ->  text you_can_eliminate_an_equivalanceText
                                              
    (DeMorganHint True f1 f2            ) ->  text you_can_apply_De_Morgan_ruleText
                                          
    (DoubleNegHint True f1 f2           ) ->  text you_can_eliminate_a_double_negationText 
                                            
    (DistrAndOverOrHint True f1 f2      ) ->  text you_can_distribute_and_over_orText
    
                                              
    (SolvedHint                         ) ->  text you_have_already_solved_the_exerciseText  
    
    (NoHint                             ) ->  text no_hint_availableText
   


{----------------------------------------------------------------------
 Function nextstep takes a formula and returns one next step.
 ---------------------------------------------------------------------}

nextstep             :: String -> String
nextstep  newFormula =  disp (nextstepp (parseFormula newFormula)) 40 "" 

nextstepp  ::  Formula -> PP_Doc
nextstepp  new  =

  case giveHint new of 
     
    (TrueFalseHint True f1 f2           ) ->  text you_can_simplify_the_formulaText
                                              >#< form2string f1 -- prettyFormula f1
                                              >-< text using_a_true_false_ruleText
                                              >#< text which_results_inText
                                              >#< form2string f2 -- prettyFormula f2  
 
    (EliminateImplicationHint True f1 f2) ->  text you_can_eliminate_the_implicationText
                                          >#< form2string f1 -- prettyFormula f1
                                          >-< text which_results_inText 
                                          >#< form2string f2 -- prettyFormula f2
    
    (EliminateEquivalenceHint True f1 f2) ->  text you_can_eliminate_the_equivalanceText
                                          >#< form2string f1 -- prettyFormula f1
                                          >-< text which_results_inText
                                          >#< form2string f2 -- prettyFormula f2
    
    (DeMorganHint True f1 f2            ) ->  text you_can_apply_De_Morgan_rule_onText
                                          >#< form2string f1 -- prettyFormula f1
                                          >-< text which_results_inText
                                          >#< form2string f2 -- prettyFormula f2 

    (DoubleNegHint True f1 f2           ) ->  text you_can_eliminate_the_double_negationText 
                                          >#< form2string f1 -- prettyFormula f1
                                          >-< text which_results_inText
                                          >#< form2string f2 -- prettyFormula f2
   
    (DistrAndOverOrHint True f1 f2      ) ->  text you_can_distribute_and_over_or_inText
                                          >#< form2string f1 -- prettyFormula f1
                                          >-< text which_results_inText
                                          >#< form2string f2 -- prettyFormula f2
    (SolvedHint                         ) ->  text you_have_already_solved_the_exerciseText  
    
    (NoHint                             ) ->  text no_hint_availableText





