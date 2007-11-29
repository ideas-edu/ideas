{---------------------------------------------------------------
 Copyright (c)        2006 - 2007 
 Johan Jeuring and Harrie Passier
----------------------------------------------------------------}
module Logic.Solver.LogicHint (giveHint, Hint) where


-- Standard Haskell libraries
import Data.List
import Data.Maybe
import Logic
import Transformation
import Strategy
import Test.QuickCheck hiding (check, ok)
import Debug.Trace
import Move
      
type Hint = Maybe (Rule LogicInContext, LogicInContext)

giveHint :: Logic -> Hint
giveHint p = 
   case nextRulesWith checkRuleName toDNF2 (inContext p) of
      (rules, _, _):_ -> Just (last rules, applyListD (init rules) $ inContext p)
      _               -> Nothing

checkRuleName :: Rule a -> Bool
checkRuleName rule = 
   let s = name rule
   in not (null s) && not (take 1 s=="_") && not ("Move" `isPrefixOf` s) && "Check" /= s

ok :: Logic -> Bool
ok p = 
   case giveHint p of
      Nothing -> isDNF p   
      Just (rule, ctx) -> ok (noContext (applyD rule ctx))
      
{-
hintToMyHint :: Hint -> MyHint
hintToMyHint hint =
   case hint of
      TrueFalseHint _ p _            -> MyRule falseTrueRuleR p
      EliminateImplicationHint _ p _ -> MyRule ruleDefImpl p
      EliminateEquivalenceHint _ p _ -> MyRule ruleDefEquiv p
      DeMorganHint _ p _             -> MyRule ruleDeMorgan p
      DoubleNegHint _ p _            -> MyRule ruleNotNot p
      DistrAndOverOrHint _ p _       -> MyRule ruleAndOverOr p
      SolvedHint                     -> MyNoHint True
      NoHint                         -> MyNoHint False

data Hint = TrueFalseHint             Bool       -- The formula can be simplified,
                                                 -- using a true-false rule.
                                      Logic    -- The formula part which can be simplified.
                                      Logic    -- The formula part after simplification.
          | EliminateImplicationHint  Bool       -- An implication can be eliminated
                                      Logic    -- The formula part containing the implication
                                      Logic    -- The formula part after elimination
          | EliminateEquivalenceHint  Bool       -- An equivalence can be eliminated
                                      Logic    -- The formula part containing the equivalence
                                      Logic    -- The formula part after elimination
          | DeMorganHint              Bool       -- De Morgan rule can be applied
                                      Logic    -- The formula part which can be rewritten using De Morgan
                                      Logic    -- The formula part after De Morgan application
          | DoubleNegHint             Bool       -- A double negation can be eliminated
                                      Logic    -- The formula part containing the double negation
                                      Logic    -- The formula part after elimination
          | DistrAndOverOrHint        Bool       -- The distribution rule can be applied (and over or)
                                      Logic    -- The formula part which can be rewritten
                                      Logic    -- The formula part after rewritting
          | SolvedHint                           -- The formula is already in dnf
          | NoHint                               -- There is no hint available
          
          deriving Show 
-}    
            

{---------------------------------------------------------------
 Function hint takes a formula as argument and produces a hint as
 result.

 The following strategy (order of steps) is used in producing a hint:
 - 1)replace all implications and all equivalences;
 - 2)push all negations, using the De Morgan laws and the law of double
   negation, inside against the variables (proposition letters);
 - Distribute 'and' over 'or'.  

 --------------------------------------------------------------}
 {-
-- Returns a subformula for which the rule is applicable
findHint :: LogicRule -> Logic -> Maybe Logic
findHint r p = fmap unLoc $ apply (somewhereTD $ check (applicable (logicRuleInContext r))) (inContext p)
 where unLoc (Loc _ x) = x
   
prop1 :: Logic -> Bool
prop1 p = containsTrueFalse p == isJust (findHint falseTrueRuleR p)

prop2 :: Logic -> Property
prop2 p = containsTrueFalse p ==> trace (show (q, q2)) $ q == q2
 where MyRule _ q = trueFalseHint p
       q2 = fromJust (findHint falseTrueRuleR p)

-- !!!!! USE THE findHint HERE !!!!!!!!!!!!!!!
giveHint' :: Logic -> MyHint
giveHint' p
   | containsTrueFalse p = trueFalseHint p   
   | containsdoubleNeg p = hintToMyHint $ doubleNegHint p
   | containsImp p       = hintToMyHint $ eliminateImpHint p
   | containsEqv p       = hintToMyHint $ eliminateEqvHint p
   | containsMorgan p    = hintToMyHint $ deMorganHint p  
   | containsAndOverOr p = hintToMyHint $ distributeHint p
   | otherwise           = MyNoHint (isDNF p)        

     
     
-- Hint: The formula can be simplified using a true false rule.   
trueFalseHint :: Logic -> MyHint
trueFalseHint f =
  case f of 
    (T :||: f)         -> hintToMyHint $ TrueFalseHint True (T :||: f) (falseTrueRule (T :||: f))
    (f :||: T)         -> hintToMyHint $ TrueFalseHint True (f :||: T) (falseTrueRule (f :||: T))
    (T :&&: f)         -> hintToMyHint $ TrueFalseHint True (T :&&: f)(falseTrueRule (T :&&: f))
    (f :&&: T)         -> hintToMyHint $ TrueFalseHint True (f :&&: T)(falseTrueRule (f :&&: T))
    (F :||: f)         -> hintToMyHint $ TrueFalseHint True (F :||: f) (falseTrueRule (F :||: f))
    (f :||: F)         -> hintToMyHint $ TrueFalseHint True (f :||: F) (falseTrueRule (f :||: F))
    (F :&&: f)         -> hintToMyHint $ TrueFalseHint True (F :&&: f)(falseTrueRule (F :&&: f))
    (f :&&: F)         -> hintToMyHint $ TrueFalseHint True (f :&&: F)(falseTrueRule (f :&&: F))
    (f1 :||: Not (f2)) -> if f1 == f2
                          then hintToMyHint $ TrueFalseHint True (f1 :||: Not (f2)) (falseTrueRule (f1 :||: Not (f2)))
                          else if containsTrueFalse f1
                               then trueFalseHint f1
                               else trueFalseHint (Not f2)                                    
    (Not (f1) :||: f2)  -> if f1 == f2
                           then hintToMyHint $ TrueFalseHint True (Not (f1) :||: f2) (falseTrueRule (Not (f1) :||: f2))
                           else if containsTrueFalse f2
                                then trueFalseHint f2
                                else trueFalseHint (Not f1) 
    (f1 :&&: Not (f2)) -> if f1 == f2
                          then hintToMyHint $ TrueFalseHint True (f1 :&&: Not (f2)) (falseTrueRule (f1 :&&: Not (f2)))
                          else if containsTrueFalse f1
                               then trueFalseHint f1
                               else trueFalseHint (Not f2) 
    (Not (f1) :&&: f2) -> if f1 == f2
                          then hintToMyHint $ TrueFalseHint True (Not (f1) :&&: f2) (falseTrueRule (Not (f1) :&&: f2))
                          else if containsTrueFalse f2
                               then trueFalseHint f2
                               else trueFalseHint (Not f1) 
    Var v              -> hintToMyHint $ TrueFalseHint False undefined undefined 
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
    (Not T)            -> hintToMyHint $ TrueFalseHint True (Not T) (falseTrueRule (Not T))
    (Not F)            -> hintToMyHint $ TrueFalseHint True (Not F) (falseTrueRule (Not F))
    Not f              -> trueFalseHint f
    T                  -> hintToMyHint $ TrueFalseHint False undefined undefined 
    F                  -> hintToMyHint $ TrueFalseHint False undefined undefined 

-- Hint: elimination of an implication
eliminateImpHint :: Logic -> Hint
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
eliminateEqvHint :: Logic -> Hint
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
deMorganHint :: Logic -> Hint
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
doubleNegHint :: Logic -> Hint
doubleNegHint f =  
  case f of
     Var v         ->  DoubleNegHint False undefined undefined
     (f1 :->: f2)  ->  if containsdoubleNeg f1 then doubleNegHint f1 else doubleNegHint f2 
     (f1 :<->: f2) ->  if containsdoubleNeg f1 then doubleNegHint f1 else doubleNegHint f2
     (f1 :&&: f2)  ->  if containsdoubleNeg f1 then doubleNegHint f1 else doubleNegHint f2
     (f1 :||: f2)  ->  if containsdoubleNeg f1 then doubleNegHint f1 else doubleNegHint f2
     Not(Not f)    ->  DoubleNegHint True (Not(Not f)) (applyD ruleNotNot (Not(Not f)))
     Not f         ->  doubleNegHint f
     T             ->  DoubleNegHint False undefined undefined
     F             ->  DoubleNegHint False undefined undefined
     
    
   

--Hint: distribute 'and' over 'or'
distributeHint :: Logic -> Hint
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

 where
   isOr :: Logic -> Bool
   isOr  (f1 :||: f2 )  = True
   isOr  _              = False

{--------------------------------------------------------------------
 The following functions detect some characteristics of proposistion
 formulae. The functions take a formula as argument and give a boolean
 as result.
--------------------------------------------------------------------}

contains :: LogicRule -> Logic -> Bool
contains lr = applicable (somewhere (logicRuleInContext lr)) . inContext

-- Detect whether a falseTrueRule can be applied.
containsTrueFalse :: Logic -> Bool
containsTrueFalse = contains falseTrueRuleR

-- Detect whether a formula contains an implication operator 
containsImp :: Logic -> Bool
containsImp = contains ruleDefImpl

-- Detect whether a formula contains an equivalence operator 
containsEqv :: Logic -> Bool
containsEqv = contains ruleDefEquiv

-- Detect whether De Morgan laws can be applied
containsMorgan :: Logic -> Bool 
containsMorgan  =  contains $ combineRules [ruleDeMorganOr, ruleDeMorganAnd]

-- Detect a double negation
containsdoubleNeg :: Logic -> Bool
containsdoubleNeg = contains ruleNotNot

-- Detect "'and' over 'or'""
containsAndOverOr :: Logic -> Bool
containsAndOverOr = contains ruleAndOverOr

-- tijdelijk
distributeRule = applyD $ combineRules [ruleAndOverOr, ruleOrOverAnd]
falseTrueRule = applyD falseTrueRuleR

deMorganRule = applyD ruleDeMorgan

ruleDeMorgan = combineRules [ruleDeMorganOr, ruleDeMorganAnd]

falseTrueRuleR = combineRules 
   [ ruleComplOr, ruleComplAnd, ruleFalseZeroOr, ruleTrueZeroOr, ruleTrueZeroAnd 
   , ruleFalseZeroAnd, ruleNotBoolConst ]

eliminateImplRule = applyD ruleDefImpl 
eliminateEqvRule = applyD ruleDefEquiv
-}

----------------------------------------------------------
-- Strategies

falseTrueS :: Strategy LogicInContext
falseTrueS = altList $ map logicRuleInContext
   [ ruleComplOr, ruleComplAnd, ruleFalseZeroOr, ruleTrueZeroOr, ruleTrueZeroAnd 
   , ruleFalseZeroAnd, ruleNotBoolConst ]
   
deMorganS :: Strategy LogicInContext
deMorganS = altList $ map logicRuleInContext
   [ruleDeMorganOr, ruleDeMorganAnd]

toDNF2 :: Strategy LogicInContext
toDNF2 = let f = toStrategy . logicRuleInContext
         in repeatS $ altList $ map somewhereTD
              [falseTrueS, f ruleNotNot, f ruleDefImpl, f ruleDefEquiv, deMorganS, f ruleAndOverOr]
              
prop :: Logic -> Bool
prop = isDNF . tr . noContext . applyD toDNF2 . inContext
 where tr a = trace (show a) a