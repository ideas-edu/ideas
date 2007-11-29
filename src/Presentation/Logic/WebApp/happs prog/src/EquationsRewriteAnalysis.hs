{--------------------------------------------------- 
Copyright (c)        2005 - 2006 
Johan Jeuring and Harrie Passier
---------------------------------------------------}


module EquationsRewriteAnalysis where


-- Standard Haskell libraries
import List
import Maybe
import Ratio
import Debug.Trace


-- GHC library
import GHC.Real


-- Equations model
import Equations        ( 
                          Equations, Equation(..), Expr(..), VarList, Var, MaybeVar,
                          conPartExpr, equalExprs, evalSolution, isBindingFor, isCon, isConVar,
                          isVar, listFactors, listTerms, numOfTermsEq, numOfFacEq, sizeExpr,
                          solveEquations, substSolution, symbolicEvalEquation, symbolicEvalExpr,
                          varPartExpr, varsEquation, VarList,varsExpr 
                        )


import EquationsUtility (
                          diffBag, diffSet, empty, exor, fst3, fst4, fth4, notEmpty, safeHead, 
                          singleton, snd3, snd4, trd3, trd4
                        )     


data RewriteAnalyse

   = EquationsRewrite   Bool     -- The substitution rule has been applied.
                        Var      -- The replaced variable.
                        Int      -- The number of the substituded equation.
                        Equation -- The correct substituded equation.
   | EquationRewriteAdd Bool     -- The addition/subtraction rule has been applied.
                        Int      -- The number of the rewritten equation.
                        Bool     -- If the rule has been (in)correctly applied.
                        Expr     -- The added/subtracted term to the right ... 
                        Expr     -- as well as to the left hand side.
   | EquationRewriteMul Bool     -- The multiplication (division) rule has been applied.
                        Int      -- The number of the rewritten equation.
                        Bool     -- If the rule has been (in)correctly applied.
                        Expr     -- The multiplication constant of the right ... 
                        Expr     -- as well as on the left hand side.
   | ExprRewrite        Bool     -- The expression rewrite rule has been applied.
                        Int      -- The number of the rewritten equation.
                        Bool     -- If the rule has been (in)correctly applied.
                        Expr     -- The expression part that has been rewritten.
                        Expr     -- The result of rule application
   | NoAnalyseResult    Bool     -- No conclusion possible.
 
   deriving Show  
    

{---------------------------------------------------------------------------------
 Function rewriteAnalyses takes two systems of linear equations (s1 on t = 1 and
 s2 on t = 2) and determines which rewrite rule has been applied:
 - substitution on the level of system of equations 
 - addition and substraction on the level of an equation
 - multiplication and division on the level of an equation
 - rewrittings on the level of an expression. 
---------------------------------------------------------------------------------}


rewriteAnalyse :: Equations -> Equations -> RewriteAnalyse


rewriteAnalyse s1 s2 = if equationsRewritten eqsRew
                       then eqsRew
                       else
                       if equationAddRewritten eqAdd
                       then eqAdd
                       else
                       if equationMulRewritten eqMul
                       then eqMul
                       else 
                       if exprRewritten exprRew
                       then exprRew
                       else NoAnalyseResult True      
   
   where

   equationsRewritten   (EquationsRewrite   b _ _ _   ) = b
   equationAddRewritten (EquationRewriteAdd b _ _ _ _ ) = b
   equationMulRewritten (EquationRewriteMul b _ _ _ _ ) = b
   exprRewritten        (ExprRewrite        b _ _ _ _ ) = b
     
   eqsRew  = equationsRewrite   s1 s2
   eqAdd   = equationRewriteAdd s1 s2
   eqMul   = equationRewriteMul s1 s2
   exprRew = exprRewrite        s1 s2

-- == REWRITE ANALYSIS FUNCTIONS ==================================================

{----------------------------------------------------------------------------------
  Rewrite analysis rule: infer application of substitution.
  Function 'equationsRewrite' determines if a substitution has been performed.
  The function takes two systems of linear equations (sle1 on t = 1 and sle2 on t = 2)
  and terurns: 
  - if the substitution rule has been applied (Bool)
  - the number of the substituded equation (Int, where "1" is the first equation)
  - if possible, the substituded equation after correct application of the substitution
    rule (Equation). The assumption is that there is one equation in the form
    of 'x = ....'.
  Condition: one substitution take place at each step 
----------------------------------------------------------------------------------}   


equationsRewrite :: Equations -> Equations -> RewriteAnalyse
equationsRewrite sle1 sle2 =
    let -- Collect the variables in both systems of equations   
        varsSLE1                  = map varsEquation sle1
        varsSLE2                  = map varsEquation sle2 
        zippedvars                = zip varsSLE1 varsSLE2
        
        -- Determine the list of removed and introduced variables, per equation
        -- represented by the tuple (removed, introduced)
        removedIntroduced         = map (\(x,y) -> (diffBag x y, diffBag y x)) zippedvars 
        
        -- Determine the place of the rewritten equation in the system (Int) and
        -- the variable replaced (Var). 
        (replacedVar, rewrittenEq)= infoSubst removedIntroduced 0 
        
        correctSubstitution       = calcCorrectSubst (fromJust replacedVar) (fromJust rewrittenEq) sle1

        -- If a substitution has taken place, the number of variables is changed 
        -- only on one side of an equation; exclusion of `x-2y=15`-> `x=15+2x`.
        oneSideChange             = onesidechange (sle1 !! (fromJust rewrittenEq))
                                                  (sle2 !! (fromJust rewrittenEq))
        -- If a substitution has taken place the number of terms remains unchanged 
        -- or increases on the left or right side (exclusion of 2x+2y=5 -> 2x=5)
        -- numOfTerms          = and (map (\(e1,e2) -> numOfTermsEq e1 <= numOfTermsEq e2) (zip sle1 sle2))
                 
        numOfFacs              = and (map (\(e1,e2) -> numOfFacEq e1 <= numOfFacEq e2) (zip sle1 sle2))
          
        
        -- Determine if a substitution has taken place, and if possible,
        -- the correct result of the substitiution. 

        ruleApplied               = (replacedVar /= Nothing) && oneSideChange && numOfFacs 
     in if ruleApplied
        then EquationsRewrite True (fromJust replacedVar) (fromJust rewrittenEq) correctSubstitution
        else EquationsRewrite False undefined undefined undefined
        
     where
    
     -- Determines the place (Int) and the replaced variable (Var) of the
     -- substituded equation in the system of equations. 
         
     infoSubst :: [(VarList,VarList)] -> Int -> (Maybe Var, Maybe Int)
     infoSubst []           n  = (Nothing, Nothing) 
     infoSubst ((x1,x2):xs) n  = if    null x1
                                 then  infoSubst xs (n+1)
                                 else  (Just (safeHead x1), Just n)
      
     safeHead xs  =  if not (null xs) then head xs else error "safeHead error in infoSubst"


     -- Calculate the correct substituded equation.
     -- (Could do better check on errors.)
     
     calcCorrectSubst :: Var -> Int -> Equations -> Equation
     calcCorrectSubst var n sle  = 
         let equation = sle!!n 
             binding  = filter (isBindingFor var) sle
             subst    = map (\(Var v:=:r) -> (v,r)) binding
         in  substSolution subst equation

     onesidechange = \(l1:=:r1) (l2:=:r2)
                   -> not ((varsExpr l1 /= varsExpr l2) && (varsExpr r1 /= varsExpr r2)) 


{----------------------------------------------------------------------------------
  Rewrite analysis rule: infer application of addition and substraction on both sides.
  Function 'termAdded' determines for each equation in a system of equations
  if on both sides a term has been added or subtracted. The function takes two
  systems of linear eqquations (sle1 on t = 1 and sle2 on t = 2) and returns:
  - if the addition/subtraction rule has been applied on an equation (Bool)
    and, if True:
    - the number of the rewritten equation (Int)
    - if the rule has been (in)correctly applied (Bool)
    - the added/subtracted term to the right as well as the left hand side (Expr). 
 ----------------------------------------------------------------------------------} 


equationRewriteAdd :: Equations -> Equations -> RewriteAnalyse 
equationRewriteAdd sle1 sle2 = 
      let rewrittenEqs    = zipWith eqRewriteAddStep sle1 sle2
          -- Check each equation on rule application 
          
          -- Determine the place of the rewritten equation in the system (Int)
          -- Assumption: zero or one application has taken place.
          -- Ps. Can be simply exented by application of 'findIndices'  
          rewrittenEq     = findIndex fst4 rewrittenEqs  
          
          -- Determine if the rule is applied
          ruleApplied     = rewrittenEq /= Nothing 
          
          -- Get the rewrite information (correctly applied (?) and the terms
          -- added to the left and right hand sides)
          (rewritten,correctRewritten,left,right) 
                          = if ruleApplied
                            then rewrittenEqs!!(fromJust rewrittenEq)
                            else (False, undefined, undefined, undefined) 
      in if ruleApplied
         then EquationRewriteAdd True (fromJust rewrittenEq) correctRewritten left right
         else EquationRewriteAdd False undefined undefined undefined undefined                                                  
      where

      -- Function termAdded takes two linear eqquations, (l1 :=: r1) on t = 1
      -- and (l2 :=: r2) on t = 2, and determines application of the addition/subtraction
      -- rule (Bool), and if true: if the rule is correctly applied (Maybe Bool) and which
      -- terms are added on the left and rigth hand sides (Maybe Expr, Maybe Expr).    
   

eqRewriteAddStep :: Equation -> Equation -> (Bool, Bool, Expr, Expr) 
eqRewriteAddStep (l1 :=: r1) (l2 :=: r2) 
          -- The left and rigth hand sides have not changed.
          | equalExprs l1 l2 && equalExprs r1 r2        = (False, undefined, undefined, undefined)
          -- The left or rigth hand sides have not changed.  
          | equalExprs l1 l2 || equalExprs r1 r2        = (False, undefined, undefined, undefined)
          -- An equal term is added on both sides
          | equalExprs diffLhs diffRhs                  = (True, True, diffLhs, diffRhs)
          -- Different constants are added on both sides 
          | isCon deltaDiff                             = (True, False, diffLhs, diffRhs)
          -- A different amount of a variable is added on both sides 
          | isConVar deltaDiff || isVar deltaDiff       = (True, False, diffLhs, diffRhs)
          -- Different variables are added on both sides 
          |  singleton (varsExpr diffLhs) && singleton (varsExpr diffRhs)
                                                        = (True, False, diffLhs, diffRhs)
          -- Diff equals a variable on one side and on the other side diff equals a constant,
          -- but the expression size on both sides isn't changed. The
          -- multiplication rule is probably applied.  
          | ( isCon diffLhs && (isConVar diffRhs || isVar diffRhs) ||
              isCon diffRhs && (isConVar diffLhs || isVar diffLhs)
            ) && ((sizeExpr l1 == sizeExpr l2) && (sizeExpr r1 == sizeExpr r2))  
                                                        = (False, undefined, undefined, undefined) 
          -- On one side a variable is added, and on one side a constant is added
          | isCon diffLhs && (isConVar diffRhs || isVar diffRhs) ||
            isCon diffRhs && (isConVar diffLhs || isVar diffLhs)
                                                        = (True, False, diffLhs, diffRhs) 
          -- The rule isn't applied properly.
          | otherwise                                   = (False, undefined, undefined, undefined)
          
          where diffLhs   = symbolicEvalExpr (l2 :-: l1)
                diffRhs   = symbolicEvalExpr (r2 :-: r1)
                deltaDiff = symbolicEvalExpr (diffLhs :-: diffRhs)

      
{----------------------------------------------------------------------------------
  Rewrite analysis rule: infer application of multiplication (or division) by a
  constant on both sides. Function 'mulByCOn' determines for each equation in a
  system of equations if both sides are multiplied by a constant.
  The function takes two systems of linear eqquations (sle1 on t = 1 and sle2
  on t = 2) and returns:
  - if the rule has been applied on an equation (Bool)
    and, if True:
    - the number of the rewritten equation (Int)
    - if the rule has been (in)correctly applied (Bool)
    - the multiplication constant of the right as well as the left hand side
      (both of type Expr). 
 ----------------------------------------------------------------------------------} 


equationRewriteMul :: Equations -> Equations -> RewriteAnalyse  
equationRewriteMul sle1 sle2 = 
      let rewrittenEqs     = zipWith mulByCon sle1 sle2
          
          -- Determine the place of the rewritten equation in the system (Int)
          -- Assumption: zero or one application has taken place
          -- Can be simply exented by application of 'findIndices'  
          rewrittenEq     = findIndex fst4 rewrittenEqs  
          
          -- Determine if the rule is applied
          ruleApplied    = rewrittenEq /= Nothing 
          
          -- Get the rewrite information (correctly applied and the terms
          -- added to the left and right hand sides)
          rewriteInfo    = rewrittenEqs!!fromJust rewrittenEq
          correctApplied = snd4 rewriteInfo 
          consLeft       = trd4 rewriteInfo    
          consRight      = fth4 rewriteInfo 
      in if ruleApplied
         then EquationRewriteMul True (fromJust rewrittenEq) correctApplied consLeft consRight
         else EquationRewriteMul False undefined undefined undefined undefined                                              


{- Function mulByCon takes two linear eqquations, (l1 :=: r1) on t = 1
   and (l2 :=: r2) on t = 2, and determines application of the multiplication
   rule (Bool), and if true: if the rule is correctly applied (Maybe Bool)
   and the constants which are multiplied on the left and rigth hand sides
   (Maybe Expr, Maybe Expr). -}    
    

mulByCon :: Equation -> Equation -> (Bool, Bool, Expr, Expr) 
mulByCon (l1 :=: r1) (l2 :=: r2)
   -- The left and rigth hand sides are not changed.  
   | equalExprs l1 l2 && equalExprs r1 r2 = (False, undefined, undefined, undefined)
   -- The left or rigth hand sides have not changed.  
   | equalExprs l1 l2 || equalExprs r1 r2 = (False, undefined, undefined, undefined)
   -- Both sides are multiplied with an equal constant 
   | isCon diffLhs && isCon  diffRhs && equalExprs diffLhs diffRhs
                                          = (True, True, diffLhs, diffRhs)
   -- Both sides are multiplied with an different constant  
   | isCon diffLhs && isCon diffRhs && not(equalExprs diffLhs diffRhs)                                                                            = (True, False, diffLhs, diffRhs)
   -- The rule isn't applied probably.
   | otherwise                            = (False, undefined, undefined, undefined)
   
   where diffLhs   = mulByCon' (l1 :/: l2)
         diffRhs   = mulByCon' (r1 :/: r2)                                                	


mulByCon' :: Expr -> Expr
mulByCon' (e1 :/: e2)   =  
   let sameVars         = empty (diffSet (nub (varsExpr e1)) (nub (varsExpr e2))) && 
                          empty (diffSet (nub (varsExpr e2)) (nub (varsExpr e1)))
       zeroCons         = (conPartExpr e1)==(0%1) && (conPartExpr e2)==(0%1)
       nonZeroCons      = (conPartExpr e1)/=(0%1) && (conPartExpr e2)/=(0%1)
       sameCons         = zeroCons || nonZeroCons
       varMultipliers   = zipWith (\(r1,v1) (r2,v2) -> r2/r1) (sortVars (varPartExpr e1))
                                                              (sortVars (varPartExpr e2))
       conMultiplier    = if zeroCons then [] else [(conPartExpr e2)/(conPartExpr e1)]
       multipliers      = varMultipliers ++ conMultiplier
       equalMultipliers = length (nub multipliers) == 1
   in  if sameVars && sameCons && equalMultipliers
       then Con (safeHead multipliers)
       else (e1 :/: e2)
       
   where
   
   sortVars xs = sortBy (\(x1,y1) (x2,y2) -> compare y1 y2) xs
   safeHead xs =  if not (null xs) then head xs else error "safeHead error in mulByCon'"
 

{- ---------------------------------------------------------------------------------
  Rewrite analysis rule: infer application of expression rewrittings.
  Function 'exprRewrite' determines for each equation in a system of equations
  if a left or right hand expression has been rewritten. The function takes two
  systems of linear equations (sle1 on t = 1 and sle2 on t = 2) and returns:
  - if the rule has been applied on an equation (Bool)
    and, if True:
    - the number of the rewritten equation (Int)
    - if the rule has been (in)correctly applied (Bool)
    - the expression part that has been rewritten (Expr) and the result
      of the rule application (Expr).
  Assumption: one rewritting in one side of one equation has taken place!! 
 --------------------------------------------------------------------------------- -} 


exprRewrite :: Equations -> Equations -> RewriteAnalyse  
exprRewrite sle1 sle2 = 
      let rewrittenEqs = zipWith eqRewriteExpr sle1 sle2
         
          -- Determine the position of the rewritten equation in the system (Int)
          -- Assumption: zero or one application has taken place.
          -- Can be extended by findIndices  
          rewrittenEq = findIndex fst rewrittenEqs  
      
          -- Determine if a rule is (correctly or incorrectly) applied, i.e. something has changed.
          -- It is possible that only some terms or factors have been swapped position
          ruleApplied = rewrittenEq /= Nothing        
      
          -- Get the rewrite information (correctly applied and the terms
          -- added to the left and right hand sides)
 
          (oldExpr, newExpr) = if ruleApplied 
                               then  diffExpr (fst(snd(snd(rewrittenEqs!!(fromJust rewrittenEq)))))
                                              (snd(snd(snd(rewrittenEqs!!(fromJust rewrittenEq)))))                                                   else (Nothing, Nothing)
         -- Only if somthing has been changed and a rewriting has taken place 
      in if (ruleApplied && (oldExpr /= Nothing || newExpr /= Nothing))
         then ExprRewrite True
                          (fromJust rewrittenEq)
                          (equalExprs (fromJust oldExpr) (fromJust newExpr))
                          (fromJust oldExpr)
                          (fromJust newExpr)
         else ExprRewrite False undefined undefined  undefined undefined
 
      where
      
      eqRewriteExpr (l1 :=: r1) (l2 :=: r2) 
        -- Only expression rewriting on one side is detected;
        -- the rigth side has priority over the left side.  
        | (l1==l2 && r1/=r2) = (True, (False, (r1, r2)))
        | (l1/=l2 && r1==r2) = (True, (True,  (l1, l2)))
        | otherwise          = (False, (undefined, (undefined, undefined)))



-- Assume *one* rewrite step has been performed

diffExpr :: Expr -> Expr -> (Maybe Expr,Maybe Expr) -- deleted from old, inserted in new
diffExpr e1 e2 = let te1  =  listTerms e1
                     te2  =  listTerms e2
                 in  diffTerms (diffBag te1 te2) (diffBag te2 te1)
 

diffTerms deleted inserted 
  | empty     deleted && empty     inserted = (Nothing, Nothing)
  | singleton deleted && singleton inserted = diffFactor (safeHead deleted) (safeHead inserted)
  | otherwise                               = (Just (addExprs deleted), Just (addExprs inserted))

  where
  
  safeHead xs  =  if not (null xs) then head xs else error "safeHead error in diffTerms" 


diffFactor e1 e2 = let fe1 = listFactors e1
                       fe2 = listFactors e2
                   in  diffFactors (diffBag fe1 fe2) (diffBag fe2 fe1)


diffFactors deleted inserted 
  | empty     deleted && empty     inserted  = (Nothing, Nothing)
  | singleton deleted && singleton inserted  = if sizeExpr del > 1 && sizeExpr ins > 1
                                               then diffExpr del ins    
                                               else (Just del, Just ins)
  | otherwise                                = (Just (mulExprs deleted), Just (mulExprs inserted)) 
 
  where
  
  safeHead xs =  if not (null xs) then head xs else error "safeHead error in diffFactors"  
  ins = safeHead inserted
  del = safeHead deleted 


addExprs        :: [Expr] -> Expr
addExprs []     = Zero
addExprs [t]    = t
addExprs (t:ts) = t :+: addExprs ts


mulExprs        :: [Expr] -> Expr
mulExprs []     = Con (1:%1)
mulExprs [t]    = t
mulExprs (t:ts) = t :*: mulExprs ts





