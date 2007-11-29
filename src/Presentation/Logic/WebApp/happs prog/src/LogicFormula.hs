{--------------------------------------------------- 
Copyright (c)        2006 - 2007 
Johan Jeuring and Harrie Passier
---------------------------------------------------}

module LogicFormula where

-- Standard Haskell library
import List
import Maybe

-- GHC library
import GHC.Real


{- The `model' for a (proposistion) formula -}


infixr 2 :<->:    --1
infixr 1 :||:     --2
infixr 1 :&&:     --3
infixr 2 :->:     --4



data Formula = Var String
             | Formula :->:  Formula        -- implication
             | Formula :<->: Formula        -- equivalence
             | Formula :&&:  Formula        -- and (conjunction)
             | Formula :||:  Formula        -- or (disjunction)
             | Not Formula                  -- not
             | T                            -- true
             | F                            -- false
             deriving (Show,Eq,Ord)        
  

type VarList    =  [Var]
type Var        =  String
type Valuation  =  [Binding]
type Binding    =  (Var, Bool)



{--------------------------------------------------------------------
 The following functions detect some characteristics of proposistion
 formulae. The functions take a formula as argument and give a boolean
 as result.
--------------------------------------------------------------------}

-- Detect whether a falseTrueRule can be applied.
containsTrueFalse :: Formula -> Bool
containsTrueFalse f =
  case f of 
    (T :||: f)         -> True
    (f :||: T)         -> True
    (T :&&: f)         -> True
    (f :&&: T)         -> True
    (F :||: f)         -> True
    (f :||: F)         -> True
    (F :&&: f)         -> True
    (f :&&: F)         -> True
    (f1 :||: Not (f2)) -> if f1 == f2 then True else containsTrueFalse f1 || containsTrueFalse (Not f2)
    (Not (f1) :||: f2) -> if f1 == f2 then True else containsTrueFalse (Not f1) || containsTrueFalse f2
    (f1 :&&: Not (f2)) -> if f1 == f2 then True else containsTrueFalse f1 || containsTrueFalse (Not f2)
    (Not (f1) :&&: f2) -> if f1 == f2 then True else containsTrueFalse (Not f1) || containsTrueFalse f2
    Var v              -> False  
    (f1 :->: f2)       -> containsTrueFalse f1 || containsTrueFalse f2 
    (f1 :<->: f2)      -> containsTrueFalse f1 || containsTrueFalse f2  
    (f1 :&&: f2)       -> containsTrueFalse f1 || containsTrueFalse f2  
    (f1 :||: f2)       -> containsTrueFalse f1 || containsTrueFalse f2  
    (Not T)            -> True
    (Not F)            -> True
    Not f              -> containsTrueFalse f 
    T                  -> False
    F                  -> False

-- Detect whether a formula contains an implication operator 
containsImp :: Formula -> Bool
containsImp f =  
  case f of
     Var v         ->  False
     (f1 :->: f2)  ->  True
     (f1 :<->: f2) ->  containsImp f1 || containsImp f2
     (f1 :&&: f2)  ->  containsImp f1 || containsImp f2
     (f1 :||: f2)  ->  containsImp f1 || containsImp f2
     Not f         ->  containsImp f
     T             ->  False
     F             ->  False


-- Detect whether a formula contains an equivalence operator 
containsEqv :: Formula -> Bool  
containsEqv f =  
  case f of
     Var v         ->  False
     (f1 :->: f2)  ->  False 
     (f1 :<->: f2) ->  True
     (f1 :&&: f2)  ->  containsEqv f1 || containsEqv f2
     (f1 :||: f2)  ->  containsEqv f1 || containsEqv f2
     Not f         ->  containsEqv f
     T             ->  False
     F             ->  False

-- Detect whether De Morgan laws can be applied
containsMorgan :: Formula -> Bool 
containsMorgan f =  
  case f of
     Var v            ->  False
     (f1 :->: f2)     ->  containsMorgan f1 || containsMorgan f2 
     (f1 :<->: f2)    ->  containsMorgan f1 || containsMorgan f2 
     (f1 :&&: f2)     ->  containsMorgan f1 || containsMorgan f2 
     (f1 :||: f2)     ->  containsMorgan f1 || containsMorgan f2 
     Not (f1 :&&: f2) ->  True
     Not (f1 :||: f2) ->  True
     Not f            ->  containsMorgan f
     T                ->  False
     F                ->  False

-- Detect a double negation
containsdoubleNeg :: Formula -> Bool 
containsdoubleNeg f =  
  case f of
     Var v         ->  False
     (f1 :->: f2)  ->  containsdoubleNeg f1 || containsdoubleNeg f2 
     (f1 :<->: f2) ->  containsdoubleNeg f1 || containsdoubleNeg f2 
     (f1 :&&: f2)  ->  containsdoubleNeg f1 || containsdoubleNeg f2 
     (f1 :||: f2)  ->  containsdoubleNeg f1 || containsdoubleNeg f2 
     Not (Not f)   ->  True
     Not f         ->  containsdoubleNeg f
     T             ->  False
     F             ->  False


-- Detect "'and' over 'or'""
containsAndOverOr :: Formula -> Bool 
containsAndOverOr f =  
  case f of
     Var v         ->  False
     (f1 :->: f2)  ->  containsAndOverOr f1 || containsAndOverOr f2 
     (f1 :<->: f2) ->  containsAndOverOr f1 || containsAndOverOr f2 
     (f1 :&&: f2)  ->  if (isOr f1 || isOr f2)
                       then True
                       else containsAndOverOr f1 || containsAndOverOr f2 
     (f1 :||: f2)  ->  containsAndOverOr f1 || containsAndOverOr f2 
     Not f         ->  containsAndOverOr f
     T             ->  False
     F             ->  False

isOr, isAnd :: Formula -> Bool
isOr  (f1 :||: f2 )  = True
isOr  _              = False
isAnd (f1 :&&: f2 )  = True
isAnd _              = False 

-- Detect whether a formulua is in DNF-form
isDnf :: Formula -> Bool
isDnf f = 
 case f of
     Var v         ->  True
     (f1 :->: f2)  ->  False 
     (f1 :<->: f2) ->  False
     (f1 :&&: f2)  ->  isAndFormula f1 && isAndFormula f2
     (f1 :||: f2)  ->  isDnf f1        && isDnf f2
     Not f         ->  isVarFormula f || isBool f
     T             ->  True
     F             ->  True

-- Detect whether a formulua is solved (in DNF-form and equal semantics)
-- f1 is the new formula, f2 the old formula.
solved :: Formula -> Formula -> Bool
solved f1 f2 = isDnf f2 && equal f1 f2

isBool T = True 
isBool F = True
isBool _ = False

-- Detect whether a formula consists only of and's
isAndFormula :: Formula -> Bool
isAndFormula (f1 :&&: f2)  = isAndFormula f1 && isAndFormula f2
isAndFormula (Not (Var v)) = True
isAndFormula (Var v)       = True
isAndFormula T             = True
isAndFormula F             = True
isAndFormula _             = False



-- Detect whether a formula is a Variable
isVarFormula :: Formula -> Bool
isVarFormula (Var v) = True
isVarFormula _       = False


isConjunction :: Formula -> Bool
isConjunction (f1 :&&: f2)  = True
isConjunction _             = False
      
isDisjunction :: Formula -> Bool
isDisjunction (f1 :||: f2) = True
isDisjunction _           = False 


{------------------------------------------------------------------
 The following functions list some information of a proposition
 formula. The functions take a formula as argument and give the 
 information as result.
------------------------------------------------------------------}


-- List the variables of a formula
varsFormula   :: Formula -> VarList
varsFormula f =  
     case f of
       Var v         ->  [v]
       (f1 :->: f2)  ->  varsFormula f1 ++ varsFormula f2
       (f1 :<->: f2) ->  varsFormula f1 ++ varsFormula f2
       (f1 :&&: f2)  ->  varsFormula f1 ++ varsFormula f2
       (f1 :||: f2)  ->  varsFormula f1 ++ varsFormula f2
       Not f         ->  varsFormula f
       T             ->  []
       F             ->  []


{----------------------------------------------------------------------
 Function evalFormula evaluates a proposition formula, given a valuation,
 to a result (boolean). The functions take a formula and a valuation 
 ass argument and returns a boolean.
------------------------------------------------------------------}


evalFormula :: Formula -> Valuation -> Bool
evalFormula f val =
     case f of
       Var v         ->    let value = lookup v val
                           in if value == Nothing
                              then error"evalFormula: var not in valuation"
                              else fromJust value 
       (f1 :->: f2)  ->    not (evalFormula f1 val) || evalFormula f2 val
       (f1 :<->: f2) ->    (evalFormula f1 val && evalFormula f2 val)
                        || (not(evalFormula f1 val) && not(evalFormula f2 val))
       (f1 :&&: f2)  ->    evalFormula f1 val && evalFormula f2 val
       (f1 :||: f2)  ->    evalFormula f1 val || evalFormula f2 val
       Not f         ->    not (evalFormula f val)
       T             ->    True
       F             ->    False

{-------------------------------------------------------------------
 Given a varList, for example [x,y], function createValuations
 creates all valuations:
 [[(x,0),(y,0)],[(x,0),(y,1)],[(x,1),(y,0)],[(x,1),(y,1)]],
 where (x,0) means: variable x equals 0.
--------------------------------------------------------------------} 

createValuations :: VarList -> [Valuation]
createValuations [] = []
createValuations xs = let l      = length (nub xs)
                          values = makeValues [] l 
                      in map (zip xs) values  
  where
  
  makeValues :: [[Bool]] -> Int -> [[Bool]]
  makeValues xs 0 = xs
  makeValues xs n = makeValues (expand xs) (n-1)                       
  
  expand :: [[Bool]] -> [[Bool]]
  expand [] = [[False],[True]]
  expand xs = let plusFalse = [False:x | x <- xs]
              in  plusFalse ++ reverse (map (map not) plusFalse)       

   
{---------------------------------------------------------------
 Function equal determines semantical equality of two formulae.
 Function equal takes two formulae as arguments. Depending on
 the number variables all valuations are produced. The two formula
 are equal if for all valuations the evaluations of both formulae
 are teh same.    
----------------------------------------------------------------}

equal :: Formula -> Formula -> Bool
equal f1 f2       = 
   let valuations = createValuations (nub (varsFormula f1 ++ varsFormula f2))
   in  and (map (\v -> evalFormula f1 v == evalFormula f2 v) valuations)               

