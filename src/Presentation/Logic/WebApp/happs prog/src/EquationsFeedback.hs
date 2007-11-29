{--------------------------------------------------- 
Copyright (c)        2005 - 2006 
Johan Jeuring and Harrie Passier
---------------------------------------------------}
module EquationsFeedback where

-- Standard Haskell libraries
import List
import Maybe (catMaybes, fromJust)

-- UU libraries
import UU.Parsing
import UU.Pretty

-- Equations parser
import EquationsParser(myShowMessages)

-- Equations model
import Equations                ( Equations(..), Equation(..), Expr(..), Var, VarList,
                                  evalSolution, isSolved, leftExpr,linear, sizeExpr,
                                  solvable, solveEquations, solvedEquations, varsEquation,
                                  varsEquations)
import EquationsUtility         ( diffSet, empty, fst3 )
import EquationsEnglishResources
import EquationsParser          ( parseEquations, parseMsgs )
import EquationsPretty          ( prettyEquations, prettyEquation, prettyExpr )
import EquationsRewriteAnalysis ( RewriteAnalyse(..), rewriteAnalyse)
import EquationsHint            ( Hint(..), giveHint )
import EquationsIndicator       ( Indicators(..), indicators )



feedback entered oldeqs = 
  let equations         =  parseEquations entered
      feedbackstring    =  disp (feedback' equations entered oldeqs) 40 ""
      indicatorsstring  =  indicatorstext equations (length (lines entered))
  in  if    null feedbackstring
      then  (False,feedbackstring,indicatorsstring) 
      else  (True,feedbackstring,indicatorsstring) 


feedback' :: Equations -> String -> String -> PP_Doc
feedback' equations entered oldeqs 
  | not (null messages) = 
      {----------------------------------------------- 
      Scan and parse the submitted equations. 
      Give feedback when a parse-error is encountered, 
      and show the corrected equations.
      -----------------------------------------------}
      text expr_syntactically_incorrect 
      >-< myShowMessages messages equations entered
  | not (all linear equations) = 
      {----------------------------------------------- 
      Analyse the equations. 
      Give feedback if they do not satisfy the 
      requirements for linear equations.
      -----------------------------------------------}
      text equations_not_lineair
      >#< text variable_times_variable
      >-< text try_again
  | not (length (nub (varsEquations equations)) == length equations) = 
      {----------------------------------------------- 
      Analyse the equations. 
      Give feedback if they do not satisfy the 
      requirements for linear equations.
      -----------------------------------------------}
      text different_num_of_vars_cq_eqs
      >-< text not_solvable 
      >-< text try_again
  | not (solvable equations) =
      text system_entered_not_solvable
  | differentSolutions (evalSolution solution) (evalSolution sol) =  
      {----------------------------------------------- 
      Calculate the solution of the equations. 
      Give feedback if the solution has changed compared
      with the previous solution. 
      
      This test should be preceeded by a test on whether
      or not the system of equations is solvable (using
      the determinant).
      ------------------------------------------------}
      case (rewriteAnalyse oldequations equations) of 

        (EquationsRewrite True v pos eq)
                           ->  text error_since_var 
                           >#< text v 
                           >#< text has_disappeared_form_eq
                           >-< prettyEquation (safeindex oldequations pos)
                           >-< text assume_tried_subst_rule
                           >-< text correctly_applying_subst_rule
                           >#< text v 
                           >#< text gives
                           >-< prettyEquation eq
                           >-< text is_this_what_you_intended
        (EquationRewriteAdd True pos False exprLeft exprRigth)
                           ->  text error_added_diff_terms_to_sides
                           >-< prettyEquation (safeindex oldequations pos) 
                           >-< text to_left_and_right
                           >#< prettyExpr 0 exprLeft
                           >#< text separation_sign
                           >#< prettyExpr 0 exprRigth
        (EquationRewriteMul True pos False leftmul rigthmul)
                           ->  text error_multiplied_eq_sides
                           >-< prettyEquation (safeindex oldequations pos)
                           >-< text with_diff_factor
                           >-< text you_have_multiplied_left_right
                           >#< prettyExpr 0 leftmul
                           >#< text separation_sign
                           >#< prettyExpr 0 rigthmul
        (ExprRewrite True pos False oldexpr newexpr)
                           ->  text error_incorrectly_rewritten_expr 
                           >-< prettyExpr 0 oldexpr
                           >-< text to_the_expr
                           >-< prettyExpr 0 newexpr
        (NoAnalyseResult True) 
                           ->  text there_is_something_wrong_1 
        otherwise          ->  text there_is_something_wrong_2
       

  | solvedEquations equations =                       
      {---------------------------------------------------- 
      The submitted set of equations is a correct solution.
      -----------------------------------------------------}
      text you_have_solved_the_eqs_correct
  | otherwise = text ""
  
  where  messages      =  parseMsgs entered
         oldequations  =  parseEquations oldeqs
         solution      =  solveEquations oldequations         
         sol           =  solveEquations equations
         safeindex     =  \l i -> if i > length l - 1 then error "feedback': safeindex" else l!!i

         differentSolutions :: [(String,Rational)] -> [(String,Rational)] -> Bool
         differentSolutions xs ys = (diffSet xs ys  ++ diffSet ys xs)/=[]



{-------------------------------------------------------------------
  Function indicatortext displays the indicator information.
 -------------------------------------------------------------------} 

indicatorstext equations len = 
  let ind = indicators equations
  in disp  (   text progressText
          >-<  indent 2 (text number_of_solved_variablesText)          >#<  pp len >|< text "):"
          >-<  indent 4 (pp (numOfVarsSolved ind))
          >-<  indent 2 (text number_of_occurrences_of_variablesText)  >#<  pp len >|< text "):"
          >-<  indent 4 (pp (numofOccurrencesOfVars ind))
          >-<  indent 2 (text size_of_lhs_expressionsText)             >#<  pp len >|< text "):"
          >-<  indent 4 (pp (sizeOfLeftHandSideExprs ind))
           )
           40 
          ""

{----------------------------------------------------------------------
 Function hint takes a system of equations and returns one hint.
 ---------------------------------------------------------------------}


hint             :: String -> String
hint  entered  =  disp (hintpp (parseEquations entered)) 40 "" 


hintpp  ::  Equations -> PP_Doc
hintpp  equations =
   case giveHint equations of 
    (ExprRule True expr)         -> text you_can_rewrite_expr
                                    >#< prettyExpr 0 expr
                                    >#< text point
    (SubstRule True eq expr var) -> text in_equations
                                    >#< prettyEquation eq
                                    >#< text you_may_replace
                                    >#< prettyExpr 0 var
                                    >#< text by
                                    >#< prettyExpr 0 expr
                                    >#< text point
    (PrepRule True eq var)       -> text you_may_rewrite
                                    >#< prettyEquation eq
                                    >#< text in_the_form_of
                                    >#< prettyExpr 0 var
                                    >#< text equals_expression
    (NoHint True)                -> text no_hint_available