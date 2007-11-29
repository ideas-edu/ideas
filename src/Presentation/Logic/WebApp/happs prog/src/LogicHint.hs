{---------------------------------------------------------------
 Copyright (c)        2006 - 2007 
 Johan Jeuring and Harrie Passier
----------------------------------------------------------------}


module LogicHint where


-- Standard Haskell libraries
import List
import Maybe


import LogicFormula 
import LogicRules
   

data Hint = TrueFalseHint             Bool       -- The formula can be simplified,
                                                 -- using a true-false rule.
                                      Formula    -- The formula part which can be simplified.
                                      Formula    -- The formula part after simplification.
          | EliminateImplicationHint  Bool       -- An implication can be eliminated
                                      Formula    -- The formula part containing the implication
                                      Formula    -- The formula part after elimination
          | EliminateEquivalenceHint  Bool       -- An equivalence can be eliminated
                                      Formula    -- The formula part containing the equivalence
                                      Formula    -- The formula part after elimination
          | DeMorganHint              Bool       -- De Morgan rule can be applied
                                      Formula    -- The formula part which can be rewritten using De Morgan
                                      Formula    -- The formula part after De Morgan application
          | DoubleNegHint             Bool       -- A double negation can be eliminated
                                      Formula    -- The formula part containing the double negation
                                      Formula    -- The formula part after elimination
          | DistrAndOverOrHint        Bool       -- The distribution rule can be applied (and over or)
                                      Formula    -- The formula part which can be rewritten
                                      Formula    -- The formula part after rewritting
          | SolvedHint                           -- The formula is already in dnf
          | NoHint                               -- There is no hint available
          
          deriving Show 

              
            

{---------------------------------------------------------------
 Function hint takes a formula as argument and produces a hint as
 result.

 The following strategy (order of steps) is used in producing a hint:
 - 1)replace all implications and all equivalences;
 - 2)push all negations, using the De Morgan laws and the law of double
   negation, inside against the variables (proposition letters);
 - Distribute 'and' over 'or'.  

 --------------------------------------------------------------}


giveHint :: Formula -> Hint
giveHint f    = if containsTrueFalse f then trueFalseHint f    else
                if containsdoubleNeg f then doubleNegHint f    else
                if containsImp f       then eliminateImpHint f else
                if containsEqv f       then eliminateEqvHint f else
                if containsMorgan f    then deMorganHint f     else
                if containsAndOverOr f then distributeHint f   else
                if isDnf f             then SolvedHint         else
                NoHint

     
-- Hint: The formula can be simplified using a true false rule.   
trueFalseHint :: Formula -> Hint
trueFalseHint f =
  case f of 
    (T :||: f)         -> TrueFalseHint True (T :||: f) (falseTrueRule (T :||: f))
    (f :||: T)         -> TrueFalseHint True (f :||: T) (falseTrueRule (f :||: T))
    (T :&&: f)         -> TrueFalseHint True (T :&&: f)(falseTrueRule (T :&&: f))
    (f :&&: T)         -> TrueFalseHint True (f :&&: T)(falseTrueRule (f :&&: T))
    (F :||: f)         -> TrueFalseHint True (F :||: f) (falseTrueRule (F :||: f))
    (f :||: F)         -> TrueFalseHint True (f :||: F) (falseTrueRule (f :||: F))
    (F :&&: f)         -> TrueFalseHint True (F :&&: f)(falseTrueRule (F :&&: f))
    (f :&&: F)         -> TrueFalseHint True (f :&&: F)(falseTrueRule (f :&&: F))
    (f1 :||: Not (f2)) -> if f1 == f2
                          then TrueFalseHint True (f1 :||: Not (f2)) (falseTrueRule (f1 :||: Not (f2)))
                          else if containsTrueFalse f1
                               then trueFalseHint f1
                               else trueFalseHint (Not f2)                                    
    (Not (f1) :||: f2)  -> if f1 == f2
                           then TrueFalseHint True (Not (f1) :||: f2) (falseTrueRule (Not (f1) :||: f2))
                           else if containsTrueFalse f2
                                then trueFalseHint f2
                                else trueFalseHint (Not f1) 
    (f1 :&&: Not (f2)) -> if f1 == f2
                          then TrueFalseHint True (f1 :&&: Not (f2)) (falseTrueRule (f1 :&&: Not (f2)))
                          else if containsTrueFalse f1
                               then trueFalseHint f1
                               else trueFalseHint (Not f2) 
    (Not (f1) :&&: f2) -> if f1 == f2
                          then TrueFalseHint True (Not (f1) :&&: f2) (falseTrueRule (Not (f1) :&&: f2))
                          else if containsTrueFalse f2
                               then trueFalseHint f2
                               else trueFalseHint (Not f1) 
    Var v              -> TrueFalseHint False undefined undefined 
    (f1 :->: f2)       -> if containsTrueFalse f1
                          then trueFalseHint f1
                          else trueFalseHint f2 
    (f1 :<->: f2)      -> if containsTrueFalse f1
                          then trueFalseHint f1
                          else trueFalseHint f2 
    (f1 :&&: f2)       -> if containsTrueFalse f1
                          then trueFalseHint f1
                          else trueFalseHint f2 
    (f1 :||: f2)       -> if containsTrueFalse f1
                          then trueFalseHint f1
                          else trueFalseHint f2  
    (Not T)            -> TrueFalseHint True (Not T) (falseTrueRule (Not T))
    (Not F)            -> TrueFalseHint True (Not F) (falseTrueRule (Not F))
    Not f              -> trueFalseHint f
    T                  -> TrueFalseHint False undefined undefined 
    F                  -> TrueFalseHint False undefined undefined 


-- Hint: elimination of an implication
eliminateImpHint :: Formula -> Hint
eliminateImpHint f =  
  case f of
     Var v         ->  EliminateImplicationHint False undefined undefined 
     (f1 :->: f2)  ->  EliminateImplicationHint True  (f1 :->: f2) (eliminateImplRule (f1 :->: f2))
     (f1 :<->: f2) ->  if containsImp f1 then eliminateImpHint f1 else eliminateImpHint f2
     (f1 :&&: f2)  ->  if containsImp f1 then eliminateImpHint f1 else eliminateImpHint f2
     (f1 :||: f2)  ->  if containsImp f1 then eliminateImpHint f1 else eliminateImpHint f2
     Not f         ->  eliminateImpHint f
     T             ->  EliminateImplicationHint False undefined undefined 
     F             ->  EliminateImplicationHint False undefined undefined  

    

-- Hint: elimination of an equivalance
eliminateEqvHint :: Formula -> Hint
eliminateEqvHint f =  
  case f of
     Var v         ->  EliminateEquivalenceHint False undefined undefined 
     (f1 :->: f2)  ->  if containsEqv f1 then eliminateEqvHint f1 else eliminateEqvHint f2
     (f1 :<->: f2) ->  EliminateEquivalenceHint True (f1 :<->: f2) (eliminateEqvRule (f1 :<->: f2))
     (f1 :&&: f2)  ->  if containsEqv f1 then eliminateEqvHint f1 else eliminateEqvHint f2
     (f1 :||: f2)  ->  if containsEqv f1 then eliminateEqvHint f1 else eliminateEqvHint f2
     Not f         ->  eliminateEqvHint f
     T             ->  EliminateEquivalenceHint False undefined undefined 
     F             ->  EliminateEquivalenceHint False undefined undefined 
    
    


-- Hint: push all negations inside against the proposition letters 
deMorganHint :: Formula -> Hint
deMorganHint f =  
  case f of
     Var v         ->  DeMorganHint False undefined undefined
     (f1 :->: f2)  ->  if containsMorgan f1 then deMorganHint f1 else deMorganHint f2
     (f1 :<->: f2) ->  if containsMorgan f1 then deMorganHint f1 else deMorganHint f2
     (f1 :&&: f2)  ->  if containsMorgan f1 then deMorganHint f1 else deMorganHint f2
     (f1 :||: f2)  ->  if containsMorgan f1 then deMorganHint f1 else deMorganHint f2
     Not f         ->  if containsMorgan (Not f) then DeMorganHint True (Not f) (deMorganRule (Not f))
                                                 else deMorganHint f
     T             ->  DeMorganHint False undefined  undefined
     F             ->  DeMorganHint False undefined  undefined

   
   

-- Hint: remove all double negations
doubleNegHint :: Formula -> Hint
doubleNegHint f =  
  case f of
     Var v         ->  DoubleNegHint False undefined undefined
     (f1 :->: f2)  ->  if containsdoubleNeg f1 then doubleNegHint f1 else doubleNegHint f2 
     (f1 :<->: f2) ->  if containsdoubleNeg f1 then doubleNegHint f1 else doubleNegHint f2
     (f1 :&&: f2)  ->  if containsdoubleNeg f1 then doubleNegHint f1 else doubleNegHint f2
     (f1 :||: f2)  ->  if containsdoubleNeg f1 then doubleNegHint f1 else doubleNegHint f2
     Not(Not f)    ->  DoubleNegHint True (Not(Not f)) (doubleNegRule (Not(Not f)))
     Not f         ->  doubleNegHint f
     T             ->  DoubleNegHint False undefined undefined
     F             ->  DoubleNegHint False undefined undefined
     
    
   

--Hint: distribute 'and' over 'or'
distributeHint :: Formula -> Hint
distributeHint f =  
  case f of
     Var v         ->  DistrAndOverOrHint False undefined undefined 
     (f1 :->: f2)  ->  if containsAndOverOr f1 then distributeHint f1 else distributeHint f2
     (f1 :<->: f2) ->  if containsAndOverOr f1 then distributeHint f1 else distributeHint f2
     (f1 :&&: f2)  ->  if containsAndOverOr f1
                       then distributeHint f1
                       else if containsAndOverOr f2
                            then distributeHint f2
                            else if isOr f1 || isOr f2
                                 then DistrAndOverOrHint True (f1 :&&: f2) (distributeRule (f1 :&&: f2))
                                 else DistrAndOverOrHint False undefined undefined 
     (f1 :||: f2)  ->  if  containsAndOverOr f1 then distributeHint f1  else distributeHint f2
     Not f         ->  if  containsAndOverOr f  then distributeHint f
                                                else DistrAndOverOrHint False undefined undefined 
     T             ->  DistrAndOverOrHint False undefined undefined 
     F             ->  DistrAndOverOrHint False undefined undefined  

    
     
    