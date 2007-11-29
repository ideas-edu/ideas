{---------------------------------------------------------------
 Copyright (c)        2006 - 2007 
 Johan Jeuring and Harrie Passier
----------------------------------------------------------------}


module EquationsHint where


-- Standard Haskell libraries
import List
import Maybe


-- Equations models
import EquationsUtility ( empty,notEmpty, rotates ,singleton,twoCombi )
import Equations        ( Equations(..), Equation(..), Expr(..), MaybeVar, Var, VarList,
                          coefficient, isBinding, isCon, isConVar, isVar,leftExpr, leftConVar,
                          listFactors, listTerms, rigthExpr, solvedEquations, varsEquation,
                          varsExpr
                        )
   


data Hint = ExprRule  Bool      -- An expression can be rewritten
                      Expr      -- The expression which can be rewritten
          | SubstRule Bool      -- A substitution can be performed
                      Equation  -- The equation in which a variable may be replaced
                      Expr      -- The expression that replaces the variable
                      Expr      -- Which variable is replaceable.
          | PrepRule  Bool      -- An equation can be prepared for substitution
                      Equation  -- Which equation can be prepared
                      Expr      -- The variable on the left hand side
          | NoHint    Bool


{-----------------------------------------------------------------
 Function giveHint takes a system of equations as argument and 
 returns one hint as result. Function giveHint called functions
 exprRule (an expression can be rewritten), substRule (a variable
 can be substituted) and prepRule (an equation can be rewritten in 
 form of `variable = expression' to prepare for substitution).
 The order in which these function are called is of importance and
 is in line with the substition method for solving a system of linear
 equations.
 ----------------------------------------------------------------}   

           
giveHint :: Equations -> Hint
giveHint eqs = if applyExprRule (exprRule eqs)
               then exprRule eqs
               else
               if applySubstRule (substRule eqs)
               then substRule eqs
               else
               if applyPrepRule (prepareSubst eqs)
               then prepareSubst eqs
               else NoHint True

    where           
    applyExprRule  (ExprRule  True _     ) = True
    applyExprRule  (ExprRule  _    _     ) = False
    applySubstRule (SubstRule True _ _ _ ) = True
    applySubstRule (SubstRule _    _ _ _ ) = False
    applyPrepRule  (PrepRule  True _ _   ) = True
    applyPrepRule  (PrepRule  _    _ _   ) = False


{---------------------------------------------------------------------
  Function exprRule takes a system of equations and determines
  whether a left or rigth handside (expression) of a equation can be
  rewritten. Possible rewrittings are: removement of brackets,
  multiplication (or division) of constants, addition (or substraction)
  of constants, and addition (or substraction) of equal variables.        
  The result of function exprRule is whether a expression may be 
  rewritten (Bool), and, if True, which part of the expression can be
  rewritten (Expr). 
  Function exprRule usus the subfunctions removeBrackets, mulCons, addCons,
  and addConVars.
  --------------------------------------------------------------------}


exprRule :: Equations  -> Hint
exprRule sle =  
 let terms          = concatMap (\(l:=:r) -> [listTerms l, listTerms r]) sle
     removebrackets = map removeBrackets terms
     mulcons        = map mulCons terms
     addcons        = map addCons terms
     addconvars     = map addConVars terms
 in  if or(map fst removebrackets)     then ExprRule True (snd(safeHead(filter fst removebrackets)))
        else if or(map fst mulcons)    then ExprRule True (snd(safeHead(filter fst mulcons)))
        else if or(map fst addcons)    then ExprRule True (snd(safeHead(filter fst addcons)))
        else if or(map fst addconvars) then ExprRule True (snd(safeHead(filter fst addconvars)))
        else ExprRule False undefined  
 where
 safeHead xs  =  if not (null xs) then head xs else error "safeHead error in hint exprRule"


-- In a term brackets can be removed
removeBrackets :: [Expr] -> (Bool, Expr)
removeBrackets terms =
 let bracketTerms = filter bracketTerm terms
 in if empty bracketTerms
    then (False, undefined)
    else (True, safeHead bracketTerms)
 where 
 bracketTerm :: Expr -> Bool          
 bracketTerm (e1 :*: (e2 :+: e3))     = True
 bracketTerm (e1 :*: (e2 :-: e3))     = True     
 bracketTerm ((e2 :+: e3) :*: e1)     = True 
 bracketTerm ((e2 :-: e3) :*: e1)     = True 
 bracketTerm ((e2 :+: e3) :/: Con c)  = True
 bracketTerm ((e2 :-: e3) :/: Con c)  = True
 bracketTerm _                        = False

 safeHead xs  = if not (null xs) then head xs else error "safeHead error in infoSubst"


-- Cons in a term can be multiplied
mulCons :: [Expr] -> (Bool, Expr)
mulCons terms =
  let twoConsTerms = (selectCons . twoOrMoreCons) (map listFactors terms)     
      consCombis   = map (\(x,y) -> (x :*: y)) (concatMap twoCombi twoConsTerms)   
  in  if empty consCombis
      then (False, undefined)
      else (True, safeHead consCombis)
  where
  selectCons = map (filter isCon)
  twoOrMoreCons  = filter (\l->(length l)>1)
  safeHead xs  = if not (null xs) then head xs else error "safeHead error in mulConst"      

-- Terms (constants) can be added
addCons :: [Expr] -> (Bool, Expr)
addCons terms      = 
  let cons      = filter isCon terms
      twoCons   = map (\(x,y) -> (x :+: y)) (twoCombi cons)
  in  if empty twoCons
      then (False, undefined)
      else (True, safeHead twoCons)
  where
  safeHead xs  = if not (null xs) then head xs else error "safeHead error in addCons"

-- Terms ('con * var') can be added
addConVars :: [Expr] -> (Bool, Expr)
addConVars terms  =
  let conVars     = filter (\t -> isConVar t || isVar t) terms
      twoConVars  = map (\(x,y) -> (x :+: y)) (twoCombi conVars)
      varsAddable = filter (\e -> singleton (nub (varsExpr e))) twoConVars
  in  if empty varsAddable 
      then (False, undefined)
      else (True, safeHead varsAddable)
  where
  safeHead xs  = if not (null xs) then head xs else error "safeHead error in addConVars"
 

{---------------------------------------------------------------------
  Function eqRule takes a system of equations and determines whether
  a equation can be rewritten in the form of ax + by = c, i.e. if constants
  can be removed form the left to the rigth hand side, and/or variables can
  be removed from the rigth to the left hand side of an equation. 
  The result of function eqRule is whether an equation may be 
  rewritten (Bool), and, if True, which terms can be replaced (Expr). 
  Function eqRule usus subfunction replaceTerms.

  N.b. Momently, function applyEqRule isn't used in the stategy!!
  --------------------------------------------------------------------}

{-

eqRule :: Equations -> Hint
eqRule sle =  
 let eqTerms      = map (\(l:=:r) -> (listTerms l, listTerms r)) sle   
     replaceable  = filter fst (map replaceTerms eqTerms)   
 in if empty replaceable
    then EqRule False undefined
    else EqRule True (head (filter fst replaceable))


-- Constants or variables can be replaced (over = sign), where constants
-- are replaced to the rigth side and variables to the left side of an equation. 
replaceTerms :: ([Expr], [Expr]) -> (Bool, Expr)
replaceTerms (l,r) =
   let replaceable = (filter isCon l) ++ filter (\t -> isConVar t || isVar t) r
   in if empty replaceable
      then (False, undefined)
      else (True, head replaceable)
-}          


{---------------------------------------------------------------------
  There are two hints for substitution. Firstly, the possibility
  of substitution is determined (function substRule). If not, the
  preparation for substitution is determined (prepareSubst).
  
  Function substRule takes a system of equations and determines the
  posibility of substitution. A substitution is possible if:
  - the left hand side of an equation is a term (term = expression) and
    this term occurs in another equation.
    Example: {2x+3y=4; 3y=x+1} --> substitute "x+1" for "3y" in "2x+3y=4")
    We call this substitution on the level of terms.  
  - an equation is a binding (variable = expression) and if the variable
    of the left hand side of the binding is a variable in another equation.
    Example: {2x+3y=4; y=x+1} --> substitute "x+1" for "y" in "2x+3y=4").
    We call this substitution on the level of variables.        
  The result of function substRule is whether a substitution can 
  take place (Bool), and, if True,  the equation in which a variable may be
  replaced (Equation), the expression that replaces the variable
  (Expr), and which variable is replaceable (Expr).
  Function substRule usus subfunction rotates.

  Function prepareSubst takes a system of equations and determines the
  posibility whether a substitution can be prepared. A preparation for 
  substitution is possible if there is an equation with two or more variables.
  Function prepareSubst determines which equation may be used for substitution,
  and rewrites that equation as a binding (variable = expression).
  The result of function prepareSubst is whether a preparation can 
  be performed (Bool), and, if True, the equation that can be rewritten as
  a binding (Equation), and the variable on the left side of
  the binding (Expr).
  --------------------------------------------------------------------}


substRule :: Equations -> Hint
substRule sle =
  let -- Determine substitution on the level of terms. 
      substs        = filter (leftConVar . fst) rts  
      posSubsts     = filter (notEmpty    . snd) (map selectEqs substs)
      -- Determine substitution on the level of variables.
      substs'       = filter (isBinding . fst) rts 
      posSubsts'    = filter (notEmpty  . snd) (map selectEqs' substs') 
  in  if notEmpty posSubsts && all (notEmpty . snd) posSubsts 
      -- A substitution on the level of terms is possible.
      then SubstRule True (safeHead (snd (safeHead posSubsts)))
                          (rigthExpr(fst (safeHead posSubsts)))
                          (leftExpr (fst (safeHead posSubsts)))
      else  
      if notEmpty posSubsts' && all (notEmpty . snd) posSubsts'
      -- A substitution on the level of a variable is possible.
      then SubstRule True (safeHead (snd (safeHead posSubsts')))
                          (rigthExpr(fst (safeHead posSubsts')))
                          (leftExpr (fst (safeHead posSubsts')))
      else SubstRule False undefined undefined undefined
      
      where 
      
      rts = rotates sle
 
      selectEqs  :: (Equation, Equations) -> (Equation, Equations)
      selectEqs     ((convar :=: e), eqs)
                  = ((convar :=: e), filter (\(l:=:r) -> convar `elem`(listTerms l ++ listTerms r )) eqs)   

      selectEqs' :: (Equation, Equations) -> (Equation, Equations)
      selectEqs'    ((Var v :=: e), eqs)
                  = ((Var v :=: e), filter (\eq -> v `elem`(varsEquation eq)) eqs)                            

      safeHead xs = if not (null xs) then head xs else error "Error safeHead substRule in Hint module"


-- Toevoegen: als 3y = expr   --> y = expr

prepareSubst :: Equations -> Hint
prepareSubst sle  =
 let  varsEqs     = map (\eq -> (eq, varsEquation eq)) sle
      -- Select those equations which have the same variables as other equations, 
      -- remove those variables that aren't in other equations,
      -- and sort the results on the number of variables in each equation.
      posSubsts   = selectEqsAndVars varsEqs
      -- Select those equations from posSubsts which have one or more variable(s)
      -- with coefficient one, and select such a variable. 
      posSubstCoefZero = selectEqAndVar posSubsts  
      -- Select the first equations from posSubsts and the first variable;
      -- there are no variables with coefficient one.
      posSubst = [ (fst(safeHead posSubsts), Var (safeHead (snd (safeHead posSubsts)))) ]               
 in if notEmpty posSubstCoefZero
    then PrepRule True (fst(safeHead posSubstCoefZero)) (snd(safeHead posSubstCoefZero)) 
    else if notEmpty posSubst
         then PrepRule True (fst(safeHead posSubst)) (snd(safeHead posSubst))
         else PrepRule False undefined undefined 
 
 where
 
 selectEqsAndVars :: [(Equation, VarList)] -> [(Equation, VarList)]   
 selectEqsAndVars xs = sort' (filter (\(x,y)-> notEmpty y)  (selectVars [] xs)) 

 
 sort' xs = sortBy (\(y,ys) (z,zs) -> compare (length (varsEquation y)) (length (varsEquation z))) xs

 
 selectVars :: [(Equation, VarList)] -> [(Equation, VarList)] -> [(Equation, VarList)]
 selectVars xs []              = xs    
 selectVars xs ((eq, vars):ys) = selectVars xs' ys 
    where   xs' = xs ++ [(eq, intersect vars (concatMap snd (xs ++ ys)))]


 selectEqAndVar :: [(Equation, VarList)] -> [(Equation, Expr)] 
 selectEqAndVar []              = []
 selectEqAndVar ((eq, vars):ys) | notEmpty oneCoefs = [(eq, Var (safeHead oneCoefs))]  
                                | otherwise         = selectEqAndVar ys
    where oneCoefs = filter (\v -> (coefficient v eq == 1 || coefficient v eq == -1 )) vars

 safeHead xs = if not (null xs) then head xs else error "Error safeHead prepareSubst in Hint module"