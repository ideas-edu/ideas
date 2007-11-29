module LogicSolver where

-- Author  : Harrie Passier
-- Date    : Augustus 11 ,2006


{- This module contains the main parts for the Logic Solver -}


{- TO DO -----------------------------------------------------------
 - DNF laten controleren door Johan of Josje (klopt deze definitie?)
 - Parser goed krijgen
 - Parser later maken met UU-bibl
 - Feedback functions maken
-------------------------------------------------------------------}



import ParserComb
import List
import Maybe


-- Prioriteiten !! ?? !!
-- Gaat ditnu goed, moet er door de parser geen haakjes geplaatst woren?

infixr 1 :<->:
infixr 2 :||:
infixr 3 :&&:
infixr 4 :->:    

-- formula -> proposition.

-- abstracte syntax

data Formula = Var String
             | Formula :->:  Formula        -- implication
             | Formula :<->: Formula        -- equivalence
             | Formula :&&:  Formula        -- and (conjunction)
             | Formula :||:  Formula        -- or (disjunction)
             | Not Formula                  -- not
             | T                            -- true
             | F  deriving (Show,Eq,Ord)    -- false    

type VarList    =  [Var]
type Var        =  String
type Valuation  =  [Binding]
type Binding    =  (Var, Bool)
 



-- Logic parser -------------------------------------

-- Reads a string according to a concrete syntax and produces and abstract syntax

rf :: String -> Formula 
rf = fst . safehead . filter (\(x,y) -> y == "") . formula
     where
     safehead xs = if length xs > 0 then head xs else error "safehead readformula" 

formula     = chainr disjunction (const (:<->:) <$> eqvSign) 
disjunction = chainr conjunction (const (:||:)  <$> orSign ) 
conjunction = chainr implication (const (:&&:)  <$> andSign)
implication = chainr basic       (const (:->:)  <$> impSign)


basic = Var <$> identifier
     <|> parenthesised formula
     <|> const T <$> symbol 'T'
     <|> const F <$> symbol 'F'
     <|> nott

nott :: Parser Char Formula
nott = (\a b c -> Not c) <$> notSign <*> spaces <*> formula 


andSign = (\ a b c d e -> "notsign") <$> spaces <*> token "and" <*> spaces
orSign  = (\ a b c d   -> "orsign")  <$> spaces <*> token "or"  <*> spaces 
impSign = (\ a b c d   -> "impsign") <$> spaces <*> token "->"  <*> spaces
eqvSign = (\ a b c d e -> "eqvsign") <$> spaces <*> token "<->" <*> spaces
notSign = (\ a b c d e -> "notsign") <$> spaces <*> token "not" <*> spaces



-- Basic functions --------------------------


-- test = (Q <-> Y) <-> (not(Q -> not Y)) 
-- test :: Formula


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

-- detect whether De Morgan laws can be applied
containsMorgan :: Formula -> Bool 
containsMorgan f =  
  case f of
     Var v         ->  False
     (f1 :->: f2)  ->  containsMorgan f1 || containsMorgan f2 
     (f1 :<->: f2) ->  containsMorgan f1 || containsMorgan f2 
     (f1 :&&: f2)  ->  containsMorgan f1 || containsMorgan f2 
     (f1 :||: f2)  ->  containsMorgan f1 || containsMorgan f2 
     Not f         ->  isdeMorganRuleFormula (Not f)
     T             ->  False
     F             ->  False

-- detect whether a double negation can be removed
containsdoubleNegRule :: Formula -> Bool 
containsdoubleNegRule f =  
  case f of
     Var v         ->  False
     (f1 :->: f2)  ->  containsdoubleNegRule f1 || containsdoubleNegRule f2 
     (f1 :<->: f2) ->  containsdoubleNegRule f1 || containsdoubleNegRule f2 
     (f1 :&&: f2)  ->  containsdoubleNegRule f1 || containsdoubleNegRule f2 
     (f1 :||: f2)  ->  containsdoubleNegRule f1 || containsdoubleNegRule f2 
     Not f         ->  isdoubleNegRuleation (Not f)
     T             ->  False
     F             ->  False


-- Detect whether a formulua is in DNF-form
isDnf :: Formula -> Bool
isDnf f = 
 case f of
     Var v         ->  True
     (f1 :->: f2)  ->  False 
     (f1 :<->: f2) ->  False
     (f1 :&&: f2)  ->  False
     (f1 :||: f2)  ->  isAndFormula f1 && isAndFormula f2
     Not f         ->  isVarFormula f
     T             ->  True
     F             ->  True

-- Detect whether a formula is an and
isAnd :: Formula  -> Bool
isAnd (f1 :&&: f2) = True
isAnd _            = False

-- Detect whether a formula is an or
isOr :: Formula  -> Bool
isOr (f1 :||: f2) = True
isOr  _           = False



-- Deze klopt niet helemaal, want nu is een var ook een and (kan misschien wel kloppen)
-- Detect whether a formula is consists only of and's
isAndFormula :: Formula -> Bool
isAndFormula (f1 :&&: f2) = isAndFormula f1 && isAndFormula f2
isAndFormula (Var v)      = True

-- Detect whether a formula is a Variable
isVarFormula :: Formula -> Bool
isVarFormula (Var v) = True
isVarFormula _       = False

-- Detect whether De Morgan laws can be applied
isdeMorganRuleFormula :: Formula -> Bool
isdeMorganRuleFormula (Not (f1 :&&: f2)) = True
isdeMorganRuleFormula (Not (f1 :||: f2)) = True
isdeMorganRuleFormula _                  = False


-- Detect whether a double negation can be removed
isdoubleNegRuleation :: Formula -> Bool
isdoubleNegRuleation (Not (Not f)) = True
isdoubleNegRuleation _             = False


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

evalFormula :: Formula -> Valuation -> Bool
evalFormula f val =
     case f of
       Var v         ->   let value = lookup v val
                          in if value == Nothing
                             then error"evalFormula: var not in valuation"
                             else fromJust value 
       (f1 :->: f2)  ->   not (evalFormula f1 val) || evalFormula f2 val
       (f1 :<->: f2) ->   (evalFormula f1 val && evalFormula f2 val)
                       || (not(evalFormula f1 val) && not(evalFormula f2 val))
       (f1 :&&: f2)  ->   evalFormula f1 val && evalFormula f2 val
       (f1 :||: f2)  ->   evalFormula f1 val || evalFormula f2 val
       Not f         ->   not (evalFormula f val)
       T             ->   True
       F             ->   False

{---------------------------------------------------------------
 Given a varList, for example [x,y], function createValuations
 create all valuations:
 [[(x,0),(y,0)],[(x,0),(y,1)],[(x,1),(y,0)],[(x,1),(y,1)]]
---------------------------------------------------------------} 

createValuations :: VarList -> [Valuation]
createValuations [] = []
createValuations xs = let l      = length (nub xs)
                          values = makeValues [] l 
                      in map (zip xs) values  
  where
  --
  makeValues :: [[Bool]] -> Int -> [[Bool]]
  makeValues xs 0 = xs
  makeValues xs n = makeValues (expand xs) (n-1)                       
  
  -- 
  expand :: [[Bool]] -> [[Bool]]
  expand [] = [[False],[True]]
  expand xs = let plusFalse = [False:x | x <- xs]
              in  plusFalse ++ reverse (map (map not) plusFalse)       

   
{---------------------------------------------------------------
 Function equal determines semantical equality of two formulae.
----------------------------------------------------------------}

equal :: Formula -> Formula -> Bool
equal f1 f2       =
   let valuations = createValuations (nub (varsFormula f1 ++ varsFormula f2))
   in  and (map (\v -> evalFormula f1 v == evalFormula f2 v) valuations)               


solved :: Formula -> Bool
solved = isDnf 
 

{---------------------------------------------------------------
 Functions to generate hints
 --------------------------------------------------------------}

-- Nog verder uitwerken
hints :: Formula -> String
hints f = if containsImp f then "Remove implication ..."
          else
          if containsEqv f then "Remove equivalence ..."
          else
          if isdeMorganRuleFormula f then "Move negations ...." 
          else
          if containsdoubleNegRule f then "Remove double negation ..."
          else   
          if not(isDnf f) then "distributeRule 'and' over 'or' ..."
          else "You have already solved the DNF-exercise!"  
        

-- nextstep :: Formula -> (Formula, Rule, Formula)
-- neststep f = 
{-

-- Hint
eliminateImpHint :: Formula -> (Bool, Maybe Formula, Maybe Formula)
eliminateImpHint f =  
  case f of
     Var v         ->  
     (f1 :->: f2)  ->  (True, Just (f1 :->: f2), Just (eliminateImplRule (f1 :->: f2)))
     (f1 :<->: f2) ->  containsImp f1 || containsImp f2
     (f1 :&&: f2)  ->  containsImp f1 || containsImp f2
     (f1 :||: f2)  ->  containsImp f1 || containsImp f2
     Not f         ->  containsImp f
     T             ->  
     F             ->  

-- Rewrite rule
eliminateImplRule :: Formula -> Maybe Formula
eliminateImplRule (f1 :->: f2) = Just (Or (Not f1) f2)
eliminateImplRule _            = Nothing


-- Hint
eliminateEqvRuleHint :: Formula -> (Bool, Maybe Formula, Maybe Formula)
eliminateEqvRuleHint f =  
  case f of
     Var v      ->  
     Imp f1 f2  ->  
     Eqv f1 f2  ->  
     And f1 f2  ->  
     Or  f1 f2  ->  
     Not f      ->  
     T          ->  
     F          ->  

-- Rewrite rule
eliminateEqvRule :: Formula -> Maybe Formula
eliminateEqvRule (f1 :<->: f2) = Just ((f1 :&&: f2) :||: ((Not f1) :&&: (Not f2)))
eliminateEqvRule _             = Nothing 


--Hint
distributeRuleHint :: Formula -> (Bool, Maybe Formula, Maybe Formula)
distributeRuleHint f =  
  case f of
     Var v         ->  Nothing
     (f1 :->: f2)  ->  
     (f1 :<->: f2) ->  
     (f1 :&&: f2)  ->  
     (f1 :||: f2)  ->  
     Not f         -> 
     T             -> 
     F             -> 

-- Rewrite rule
distributeRule :: Formula -> Maybe Formula
distributeRule (f1 :&&: (f2 :||: f3)) = Just ((f1 :&&: f2) :||: (f1 :&&: f3))
distributeRule ((f1 :||: f2) :&&: f3) = Just ((f3 :&&: f1) :||: (f3 :&&: f2))
distributeRule _                      = Nothing


-- Hint
deMorganRuleHint :: Formula -> (Bool, Maybe Formula, Maybe Formula)
deMorganRuleHint f =  
  case f of
     Var v         ->  
     (f1 :->: f2)  ->  
     (f1 :<->: f2) ->  
     (f1 :&&: f2)  ->  
     (f1 :||: f2)  ->  
     Not f         ->  
     T             ->  
     F             ->  

-- Rewrite rule
deMorganRule :: Formula -> Maybe Formula
deMorganRule (Not (f1 :&&: f2)) = Just ((Not f1) :||: (Not f2))
deMorganRule (Not (f1 :||: f2)) = Just ((Not f1) :&&: (Not f2))
deMorgen _                      = Nothing
 

-- Hint
doubleNegRuleHint :: Formula -> (Bool, Maybe Formula, Maybe Formula)
doubleNegRuleHint f =  
  case f of
     Var v         ->  
     (f1 :->: f2)  ->  
     (f1 :<->: f2) ->  
     (f1 :&&: f2)  ->  
     (f1 :||: f2)  ->  
     Not f         -> 
     F             ->  

-- Rewrite rule
doubleNegRule :: Formula -> Maybe Formula
doubleNegRule (Not (Not f)) = Just f
doubleNegRule _             = Nothing 

-}

f = ((Var "y") :&&: (Var "x")) :<->: ((Var "x") :||: (Var "y"))
g = ((Var "y") :&&: (Var "x")) :<->: ((Var "x") :&&: (Var "y"))


-- Wat als T or False op top-niveau wordt toegvoegd met or (bv)??

-- Returns the difference between two formulae.
diff :: (Formula, Formula) -> Maybe (Formula, Formula)
diff (f, g) = 
  case (f, g) of   
    (T, T)                     -> Nothing
    (F, F)                     -> Nothing
    (T, F)                     -> Just (T, F)
    (F, T)                     -> Just (F, T)
    (Var a, Var b)             -> if a == b then Nothing else Just (Var a, Var b)
   -- Onderstaande kan wrs d.m.v. 1 functie
    (f1 :&&: f2, f3 :&&: f4)   -> if diff (f1, f3) == Nothing && diff (f2, f4) == Nothing
                                  then Nothing
                                  else 
                                  if diff (f1, f3) /= Nothing && diff (f2, f4) == Nothing
                                  then diff (f1, f3)
                                  else
                                  if diff (f1, f3) == Nothing && diff (f2, f4) /= Nothing
                                  then diff (f2, f4)
                                  else Just (f, g)
    (f1 :->: f2, f3 :->: f4)   -> if diff (f1, f3) == Nothing && diff (f2, f4) == Nothing
                                  then Nothing
                                  else 
                                  if diff (f1, f3) /= Nothing && diff (f2, f4) == Nothing
                                  then diff (f1, f3)
                                  else
                                  if diff (f1, f3) == Nothing && diff (f2, f4) /= Nothing
                                  then diff (f2, f4)
                                  else Just (f, g) 
    (f1 :<->: f2, f3 :<->: f4) -> if diff (f1, f3) == Nothing && diff (f2, f4) == Nothing
                                  then Nothing
                                  else 
                                  if diff (f1, f3) /= Nothing && diff (f2, f4) == Nothing
                                  then diff (f1, f3)
                                  else
                                  if diff (f1, f3) == Nothing && diff (f2, f4) /= Nothing
                                  then diff (f2, f4)
                                  else Just (f, g)
    (f1 :||: f2, f3 :||: f4)   -> if diff (f1, f3) == Nothing && diff (f2, f4) == Nothing
                                  then Nothing
                                  else 
                                  if diff (f1, f3) /= Nothing && diff (f2, f4) == Nothing
                                  then diff (f1, f3)
                                  else
                                  if diff (f1, f3) == Nothing && diff (f2, f4) /= Nothing
                                  then diff (f2, f4)
                                  else Just (f, g)
    (Not f1, Not f2)           -> if diff (f1, f2) == Nothing 
                                  then Nothing
                                  else Just (f1, f2)

    ( _ , _)                   -> Just (f, g)



{------------------------------------------------------------------
 Feedback functions
 Assumption: 
 (1) Feedback functions **are called** when a semantical error
     is detected. The case of correct rule application is not included;
     only the incorrect applications are included.
 (2) ...
 ------------------------------------------------------------------}

-- Result type

data Feedback = FB { ruleName         :: String           
                   , ruleApplied      :: Bool
                   , correctApplied   :: Bool    {- or undefined -}
                   , oldFormula       :: Formula {- or undefined -}
                   , submittedFormula :: Formula {- or undefined -} 
                   , correctFormula   :: Formula {- or undefined -} 
                   } deriving (Show, Eq)
            
type OldFormula = Formula 
type NewFormula = Formula

commutativity :: OldFormula -> NewFormula -> Maybe Feedback
commutativity (f1 :&&: f2) new =
    case new of
        (f3 :&&: f4)         -> if f1 == f4 && f2 == f3
                                then Just FB { ruleName         = "commutativity (and)"
                                             , ruleApplied      = True
                                             , correctApplied   = True
                                             , oldFormula       = f1 :&&: f2
                                             , submittedFormula = new 
                                             , correctFormula   = undefined
                                             }
                                else
                                if f1 == f4 || f2 == f3
                                then Just FB { ruleName         = "commutativity (and)"
                                             , ruleApplied      = True
                                             , correctApplied   = False
                                             , oldFormula       = f1 :&&: f2
                                             , submittedFormula = new 
                                             , correctFormula   = f2 :&&: f1
                                             }
                                else Nothing
        (f3 :||: f4)         -> if f1 == f4 && f2 == f3
                                then Just FB { ruleName         = "commutativity (and)"
                                             , ruleApplied      = True
                                             , correctApplied   = False
                                             , oldFormula       = f1 :&&: f2
                                             , submittedFormula = new 
                                             , correctFormula   = f2 :&&: f1
                                             }
                                else Nothing
        ( _ )                -> Nothing 
                                  
commutativity (f1 :||: f2) new  =
    case new of
        (f3 :||: f4)           -> if f1 == f4 && f2 == f3
                                then Just FB { ruleName         = "commutativity (or)"
                                             , ruleApplied      = True
                                             , correctApplied   = True
                                             , oldFormula       = f1 :||: f2
                                             , submittedFormula = new 
                                             , correctFormula   = new
                                             }
                                else
                                if f1 == f4 || f2 == f3
                                then Just FB { ruleName         = "commutativity (or)"
                                             , ruleApplied      = True
                                             , correctApplied   = False
                                             , oldFormula       = f1 :||: f2
                                             , submittedFormula = new 
                                             , correctFormula   = f2 :||: f1
                                             }
                                else Nothing
        (f3 :&&: f4)         -> if f1 == f4 && f2 == f3
                                then Just FB { ruleName         = "commutativity (or)"
                                             , ruleApplied      = True
                                             , correctApplied   = False
                                             , oldFormula       = f1 :||: f2
                                             , submittedFormula = new 
                                             , correctFormula   = f2 :||: f1
                                             }
                                else Nothing
        ( _ )                -> Nothing 

commutativity _ _             = Nothing






