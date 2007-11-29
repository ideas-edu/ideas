{--------------------------------------------------- 
Copyright (c)        2005 - 2006 
Johan Jeuring and Harrie Passier
---------------------------------------------------}

-- HIER RULES HET WERK LATEN DOEN : IN HET CORRECT VELD !!


module LogicRewriteAnalysis where


-- Standard Haskell libraries
import List
import LogicFormula
import Maybe
import Ratio
import Debug.Trace


-- GHC library
-- import GHC.Real


-- Equations model
import LogicFormula     


-- Result type of feedback
data Feedback = FB { ruleName         :: RuleName
                   , ruleApplied      :: Bool    {- or undefined -}       
                   , correctApplied   :: Bool    {- or undefined -}
                   , removed          :: Formula {- or undefined -}
                   , introduced       :: Formula {- or undefined -} 
                   , correct          :: Formula {- or undefined -} 
                   } deriving Show
                  
data RuleInfo = RI { ruleInfo         :: RuleName } 

data RuleName = TrueFalse 
              | EliminateImplication
              | EliminateEquivalence
              | DeMorgan
              | RemoveDoubleNeg
              | DistrAndoverOr
              | Commutativity
              | CommutativityImplication
              | AssociativityImp
              | AssociativityEqv
              | AssociativityAnd
              | AssociativityOr
              | Associativity
              | Idempotency
              | NoRuleDetected
              | NoDiff
              
              deriving (Eq, Show)

{---------------------------------------------------------------------------------
 Function rewriteAnalyses
---------------------------------------------------------------------------------}

old = (Not (Var "q") :||: Not(Var "s"  :||: Var "t" )) :&&: (Var "s" :||:  Var "t")


new = (Not (Var "q") :&&: (Var "s" :||:  Var "t")) :||: ( Not (Var "s"  :||: Var "t" ) :&&: (Var "s" :||:  Var "t"))


rewriteAnalyse :: Formula -> Formula -> Feedback
rewriteAnalyse old new =
    if isNothing diffOldNew 
    then FB { ruleName       = NoDiff
            , ruleApplied    = undefined
            , correctApplied = undefined
            , removed        = undefined
            , introduced     = undefined 
            , correct        = undefined
            } 
    else
    if applied truefalseRewr 
    then fromJust truefalseRewr
    else
    if applied implRewr 
    then fromJust implRewr
    else
    if applied eqvRewr 
    then fromJust eqvRewr
    else    
    if applied deMoRewr 
    then fromJust deMoRewr
    else 
    if applied negRewr  
    then fromJust negRewr
    else
    if applied distrRewr 
    then fromJust distrRewr
    else
    if applied commRewr 
    then fromJust commRewr
    else
    if applied assoRewr 
    then fromJust assoRewr
    else FB { ruleName       = NoRuleDetected
            , ruleApplied    = undefined
            , correctApplied = undefined
            , removed        = rem
            , introduced     = int
            , correct        = undefined
            }   
    
    where 
    
    diffOldNew = diff (old, new) 

    -- rem = removed, int = introduced
    rem       = if isNothing diffOldNew then undefined else fst (fromJust diffOldNew)
    int       = if isNothing diffOldNew then undefined else snd (fromJust diffOldNew)

    -- Is a rule (probably) applied?
    applied r = if isNothing r then False else ruleApplied (fromJust r) 
    
    -- Rewrite analysis information    
    truefalseRewr = truefalse           rem int 
    implRewr      = eliminateImpl       rem int
    eqvRewr       = eliminateEqv        rem int
    deMoRewr      = deMorgan            rem int
    negRewr       = doubleNeg           rem int
    distrRewr     = distributeAndOverOr rem int
    commRewr      = commutativity       rem int
    assoRewr      = associativity       rem int



{--------------------------------------------------------------------
 Feedback functions
 Assumption: 
 - Feedback functions are called only if a semantical error is detected.
   The case of correct rule application is not included;
   only the incorrect applications are included.
---------------------------------------------------------------------}



{--------------------------------------------------------------------
 Function truefalse tests whether a mistake is made during 
 application of this rule.
 Example truefalse rule:  (T :||: f) -> T  = correct
         truefalse rule:  (T :||: f) -> F  = incorrect
--------------------------------------------------------------------} 

truefalse :: Formula -> Formula -> Maybe Feedback
truefalse (T :||: f)  T     = Just FB { ruleName         = TrueFalse
                                      , ruleApplied      = True
                                      , correctApplied   = True
                                      , removed          = (T :||: f)
                                      , introduced       = T
                                      , correct          = T
                                      }
truefalse (T :||: f)  F     = Just FB { ruleName         = TrueFalse
                                      , ruleApplied      = True
                                      , correctApplied   = False
                                      , removed          = (T :||: f)
                                      , introduced       = F
                                      , correct          = T                                 
                                      } 
truefalse (f :||: T) T      = Just FB { ruleName         = TrueFalse
                                      , ruleApplied      = True
                                      , correctApplied   = True
                                      , removed          = (f :||: T)
                                      , introduced       = T
                                      , correct          = T                                 
                                      }    
truefalse (f :||: T) F     = Just FB  { ruleName         = TrueFalse
                                      , ruleApplied      = True
                                      , correctApplied   = False
                                      , removed          = (f :||: T)
                                      , introduced       = F
                                      , correct          = T                                 
                                      }      
truefalse (T :&&: f)  T     = Just FB { ruleName         = TrueFalse
                                      , ruleApplied      = True
                                      , correctApplied   = False
                                      , removed          = (T :&&: f)
                                      , introduced       = T
                                      , correct          = f
                                      }
truefalse (T :&&: f)  F     = Just FB { ruleName         = TrueFalse
                                      , ruleApplied      = True
                                      , correctApplied   = False
                                      , removed          = (T :&&: f)
                                      , introduced       = F
                                      , correct          = f                                
                                      } 
truefalse (f :&&: T) T      = Just FB { ruleName         = TrueFalse
                                      , ruleApplied      = True
                                      , correctApplied   = False
                                      , removed          = (f :&&: T)
                                      , introduced       = T
                                      , correct          = f                                 
                                      }    
truefalse (f :&&: T) F     = Just FB  { ruleName         = TrueFalse
                                      , ruleApplied      = True
                                      , correctApplied   = False
                                      , removed          = (f :&&: T)
                                      , introduced       = F
                                      , correct          = f                                 
                                      } 

truefalse (F :||: f)  T     = Just FB { ruleName         = TrueFalse
                                      , ruleApplied      = True
                                      , correctApplied   = False
                                      , removed          = (T :||: f)
                                      , introduced       = T
                                      , correct          = f
                                      }
truefalse (F :||: f)  F     = Just FB { ruleName         = TrueFalse
                                      , ruleApplied      = True
                                      , correctApplied   = False
                                      , removed          = (T :||: f)
                                      , introduced       = F
                                      , correct          = f                                 
                                      } 
truefalse (f :||: F) T      = Just FB { ruleName         = TrueFalse
                                      , ruleApplied      = True
                                      , correctApplied   = False
                                      , removed          = (f :||: T)
                                      , introduced       = T
                                      , correct          = f                                 
                                      }    
truefalse (f :||: F) F     = Just FB  { ruleName         = TrueFalse
                                      , ruleApplied      = True
                                      , correctApplied   = False
                                      , removed          = (f :||: T)
                                      , introduced       = F
                                      , correct          = f                                
                                      }      
truefalse (F :&&: f)  T     = Just FB { ruleName         = TrueFalse
                                      , ruleApplied      = True
                                      , correctApplied   = False
                                      , removed          = (T :&&: f)
                                      , introduced       = T
                                      , correct          = F
                                      }
truefalse (F :&&: f)  F     = Just FB { ruleName         = TrueFalse
                                      , ruleApplied      = True
                                      , correctApplied   = True
                                      , removed          = (T :&&: f)
                                      , introduced       = F
                                      , correct          = F                                
                                      } 
truefalse (f :&&: F) T      = Just FB { ruleName         = TrueFalse
                                      , ruleApplied      = True
                                      , correctApplied   = False
                                      , removed          = (f :&&: T)
                                      , introduced       = T
                                      , correct          = F                                 
                                      }    
truefalse (f :&&: F) F     = Just FB  { ruleName         = TrueFalse
                                      , ruleApplied      = True
                                      , correctApplied   = True
                                      , removed          = (f :&&: T)
                                      , introduced       = F
                                      , correct          = F                                 
                                      } 

truefalse (f1 :||: Not (f2)) T = if f1 == f2 then Just FB { ruleName         = TrueFalse
                                                          , ruleApplied      = True
                                                          , correctApplied   = True
                                                          , removed          = (f1 :||: Not (f2))
                                                          , introduced       = T
                                                          , correct          = T                                 
                                                          }
                                             else Nothing 
truefalse (f1 :||: Not (f2)) F = if f1 == f2 then Just FB { ruleName         = TrueFalse
                                                          , ruleApplied      = True
                                                          , correctApplied   = False
                                                          , removed          = (f1 :||: Not (f2))
                                                          , introduced       = F
                                                          , correct          = T                                 
                                                          }
                                             else Nothing 

truefalse (Not (f1) :||: f2) T = if f1 == f2 then Just FB { ruleName         = TrueFalse
                                                          , ruleApplied      = True
                                                          , correctApplied   = True
                                                          , removed          = (Not (f1) :||: f2)
                                                          , introduced       = T
                                                          , correct          = T                                 
                                                          }
                                             else Nothing 
truefalse (Not (f1) :||: f2) F = if f1 == f2 then Just FB { ruleName         = TrueFalse
                                                          , ruleApplied      = True
                                                          , correctApplied   = False
                                                          , removed          = (Not (f1) :||: f2)
                                                          , introduced       = F
                                                          , correct          = T                                 
                                                          }
                                             else Nothing 
                   
truefalse (f1 :&&: Not (f2)) T = if f1 == f2 then Just FB { ruleName         = TrueFalse
                                                          , ruleApplied      = True
                                                          , correctApplied   = False
                                                          , removed          = (f1 :&&: Not (f2))
                                                          , introduced       = T
                                                          , correct          = F                                 
                                                          }
                                             else Nothing 
truefalse (f1 :&&: Not (f2)) F = if f1 == f2 then Just FB { ruleName         = TrueFalse
                                                          , ruleApplied      = True
                                                          , correctApplied   = True
                                                          , removed          = (f1 :&&: Not (f2))
                                                          , introduced       = F
                                                          , correct          = F                                 
                                                          }
                                             else Nothing 

truefalse (Not (f1) :&&: f2) T = if f1 == f2 then Just FB { ruleName         = TrueFalse
                                                          , ruleApplied      = True
                                                          , correctApplied   = False
                                                          , removed          = (Not (f1) :&&: f2)
                                                          , introduced       = T
                                                          , correct          = F                                 
                                                          }
                                             else Nothing 
truefalse (Not (f1) :&&: f2) F = if f1 == f2 then Just FB { ruleName         = TrueFalse
                                                          , ruleApplied      = True
                                                          , correctApplied   = True
                                                          , removed          = (Not (f1) :&&: f2)
                                                          , introduced       = F
                                                          , correct          = F                                 
                                                          }
                                             else Nothing 

truefalse (Not T) F      = Just FB { ruleName         = TrueFalse
                                   , ruleApplied      = True
                                   , correctApplied   = True
                                   , removed          = Not T
                                   , introduced       = F
                                   , correct          = F                                 
                                   }    

truefalse (Not T) T      = Just FB { ruleName         = TrueFalse
                                   , ruleApplied      = True
                                   , correctApplied   = False
                                   , removed          = Not T
                                   , introduced       = T
                                   , correct          = F                                 
                                   }    

truefalse (Not F) T      = Just FB { ruleName         = TrueFalse
                                   , ruleApplied      = True
                                   , correctApplied   = True
                                   , removed          = Not F
                                   , introduced       = T
                                   , correct          = T                                 
                                   }    

truefalse (Not F) F      = Just FB { ruleName         = TrueFalse
                                   , ruleApplied      = True
                                   , correctApplied   = False
                                   , removed          = Not F
                                   , introduced       = F
                                   , correct          = T                                
                                   }    

truefalse _ _ = Nothing

{--------------------------------------------------------------------
 Function eliminateImpl tests whether a mistake is made during 
 application of this elimination rule.
 Eliminate implication rule:  (f1 :->: f2) -> (Not f1) :||: f2
--------------------------------------------------------------------} 

eliminateImpl :: Formula -> Formula -> Maybe Feedback

eliminateImpl ((f1 :->: f2) :->: f3) (f4 :->: (f5 :->: f6)) =
                                 if f1 == f4 && f2 == f5 && f3 == f6 
                                 then  Just FB { ruleName        = AssociativityImp
                                              , ruleApplied      = True
                                              , correctApplied   = True
                                              , removed          = ((f1 :->: f2) :->: f3)
                                              , introduced       = (f4 :->: (f5 :->: f6))
                                              , correct          = ((f1 :->: f2) :->: f3)
                                              }
                                 else Nothing
                               
eliminateImpl (f1 :->: (f2 :->: f3)) ((f4 :->: f5) :->: f6) =
                                 if f1 == f4 && f2 == f5 && f3 == f6 
                                 then  Just FB { ruleName        = AssociativityImp
                                              , ruleApplied      = True
                                              , correctApplied   = True
                                              , removed          = (f1 :->: (f2 :->: f3))
                                              , introduced       = ((f4 :->: f5) :->: f6)
                                              , correct          = (f1 :->: (f2 :->: f3))
                                              }
                                 else Nothing                 



eliminateImpl (f1 :->: f2) (f3 :->: f4) =  if f1 == f4 && f2 == f3 
                                           then  Just FB { ruleName = CommutativityImplication
                                               , ruleApplied        = True
                                               , correctApplied     = False
                                               , removed            = f1 :->: f2
                                               , introduced         = f3 :->: f4
                                               , correct            = f1 :->: f2
                                               }
                                           else Nothing
       


eliminateImpl (f1 :->: f2) new  = Just FB { ruleName       = EliminateImplication
                                          , ruleApplied    = True
                                          , correctApplied = False
                                          , removed        = (f1 :->: f2)
                                          , introduced     = new 
                                          , correct        = (Not f1) :||: f2
                                          }
eliminateImpl _ _               = Nothing


{------------------------------------------------------------
 Function eliminateEqv tests whether a mistake is made during 
 application of this elimination rule.
 Eliminate equivalance rule:
 (f1 :<->: f2) -> (f1 :&&: f2) :||: ((Not f1) :&&: (Not f2))
-------------------------------------------------------------} 

eliminateEqv :: Formula -> Formula -> Maybe Feedback

eliminateEqv (f1 :<->: f2) new  = Just FB { ruleName       = EliminateEquivalence
                                          , ruleApplied    = True
                                          , correctApplied = False
                                          , removed        = (f1 :<->: f2)
                                          , introduced     = new 
                                          , correct        = (f1 :&&: f2) :||: ((Not f1) :&&: (Not f2))
                                          }
eliminateEqv _ _               = Nothing

{------------------------------------------------------------
 Function deMorgan tests whether a mistake is made during 
 application of this rule.
 De Morgan rule: (Not (f1 :&&: f2)) -> (Not f1) :||: (Not f2)
                 (Not (f1 :||: f2)) -> (Not f1) :&&: (Not f2)
-------------------------------------------------------------} 

deMorgan           :: Formula -> Formula -> Maybe Feedback

deMorgan (Not (Not f1 :&&: f2)) (f3 :&&: f4) = 
                           if f1 == f3 &&  f2 == f4
                           then Just FB { ruleName       = RemoveDoubleNeg
                                        , ruleApplied    = True
                                        , correctApplied = False
                                        , removed        = (Not (Not f1 :&&: f2))
                                        , introduced     = (f3 :&&: f4)
                                        , correct        = (Not (Not f1 :&&: f2))
                                        }
                           else Nothing

deMorgan (Not (Not f1 :||: f2)) (f3 :||: f4) =
                           if f1 == f3 &&  f2 == f4
                           then Just FB { ruleName       = RemoveDoubleNeg
                                        , ruleApplied    = True
                                        , correctApplied = False
                                        , removed        = (Not (Not f1 :||: f2))
                                        , introduced     = (f3 :||: f4)
                                        , correct        = (Not (Not f1 :||: f2))
                                        }
                           else Nothing

deMorgan (Not (Not f1 :->: f2)) (f3 :->: f4) =
                           if f1 == f3 &&  f2 == f4
                           then Just FB { ruleName       = RemoveDoubleNeg
                                        , ruleApplied    = True
                                        , correctApplied = False
                                        , removed        = (Not (Not f1 :->: f2))
                                        , introduced     = (f3 :->: f4)
                                        , correct        = (Not (Not f1 :->: f2))
                                        }
                           else Nothing

deMorgan (Not (Not f1 :<->: f2)) (f3 :->: f4) =
                           if f1 == f3 &&  f2 == f4
                           then  Just FB { ruleName       = RemoveDoubleNeg
                                        , ruleApplied    = True
                                        , correctApplied = False
                                        , removed        = (Not (Not f1 :<->: f2))
                                        , introduced     = (f3 :<->: f4)
                                        , correct        = (Not (Not f1 :<->: f2)) 
                                        }
                           else Nothing

deMorgan (Not (f1 :&&: f2)) new  = Just FB { ruleName       = DeMorgan
                                           , ruleApplied    = True
                                           , correctApplied = False
                                           , removed        = (Not (f1 :&&: f2))
                                           , introduced     = new 
                                           , correct        = (Not f1) :||: (Not f2)
                                          }
deMorgan (Not (f1 :||: f2)) new  = Just FB { ruleName       = DeMorgan
                                           , ruleApplied    = True
                                           , correctApplied = False
                                           , removed        = (Not (f1 :||: f2))
                                           , introduced     = new 
                                           , correct        = (Not f1) :&&: (Not f2)
                                          }

deMorgan _ _                     = Nothing


{------------------------------------------------------------
 Function doubleNeg tests whether a mistake is made during 
 application of this rule.
 Double negation rule: (Not (Not f)) -> f
-------------------------------------------------------------} 
 
doubleNeg          :: Formula -> Formula -> Maybe Feedback

doubleNeg  (Not (Not f)) new  = Just FB { ruleName       = RemoveDoubleNeg
                                        , ruleApplied    = True
                                        , correctApplied = False
                                        , removed        = (Not (Not f))
                                        , introduced     = new 
                                        , correct        = f
                                        }

doubleNeg _ _                 = Nothing
 

{------------------------------------------------------------
 Function distributeAndOverOr tests whether a mistake is made
 during application of this rule.
 Distribute and-over-or rule: 
      (f1 :&&: (f2 :||: f3)) -> (f1 :&&: f2) :||: (f1 :&&: f3)
      ((f2 :||: f3) :&&: f1) -> (f1 :&&: f2) :||: (f1 :&&: f3)
-------------------------------------------------------------} 

distributeAndOverOr :: Formula -> Formula -> Maybe Feedback

distributeAndOverOr  (f1 :&&: (f2 :||: f3)) new  = Just FB { ruleName       = DistrAndoverOr
                                                           , ruleApplied    = True
                                                           , correctApplied = False
                                                           , removed        = (f1 :&&: (f2 :||: f3))
                                                           , introduced     = new 
                                                           , correct        = (f1 :&&: f2) :||: (f1 :&&: f3)
                                                           }
distributeAndOverOr  ((f2 :||: f3) :&&: f1) new  = Just FB { ruleName       = DistrAndoverOr
                                                           , ruleApplied    = True
                                                           , correctApplied = False
                                                           , removed        = ((f2 :||: f3) :&&: f1)
                                                           , introduced     = new 
                                                           , correct        = (f2 :&&: f1) :||: (f3 :&&: f1)
                                                           }
distributeAndOverOr _ _                          = Nothing



{------------------------------------------------------------
 Function commutativity tests whether a mistake is made during 
 application of the commutativity rule.
 Commutativity rule: (f1 :&&: f2) -> (f2 :&&: f1)
                     (f1 :||: f2) -> (f2 :||: f1)
-------------------------------------------------------------} 

commutativity :: Formula -> Formula -> Maybe Feedback

commutativity (f1 :&&: f2) new =
    case new of
        (f3 :&&: f4)         -> if f1 /= f4 || f2 /= f3
                                then Just FB { ruleName         = Commutativity
                                             , ruleApplied      = True
                                             , correctApplied   = False
                                             , removed          = f1 :&&: f2
                                             , introduced       = new 
                                             , correct          = f2 :&&: f1
                                             }
                                else Nothing
        (f3 :||: f4)         -> if f1 == f4 && f2 == f3
                                then Just FB { ruleName         = Commutativity
                                             , ruleApplied      = True
                                             , correctApplied   = False
                                             , removed          = f1 :&&: f2
                                             , introduced       = new 
                                             , correct          = f2 :&&: f1
                                             }
                                else Nothing
        otherwise            -> Nothing 
                              
commutativity (f1 :||: f2) new  =
    case new of
        (f3 :||: f4)         -> if f1 /= f4 || f2 /= f3
                                then Just FB { ruleName         = Commutativity
                                             , ruleApplied      = True
                                             , correctApplied   = False
                                             , removed          = f1 :||: f2
                                             , introduced       = new 
                                             , correct          = f2 :||: f1
                                             }
                                else Nothing
        (f3 :&&: f4)         -> if f1 == f4 && f2 == f3
                                then Just FB { ruleName         = Commutativity
                                             , ruleApplied      = True
                                             , correctApplied   = False
                                             , removed          = f1 :||: f2
                                             , introduced       = new 
                                             , correct          = f2 :||: f1
                                             }
                                else Nothing
        otherwise            -> Nothing 

commutativity _ _             =  Nothing


{------------------------------------------------------------
 Function associativity tests whether a mistake is made during 
 application of the associativity rule.
 Associativity rule:
            ((f1 :&&: f2) :&&: F3) <-> (f1 :&&: (f2 :&&: f3))
            ((f1 :||: f2) :||: F3) <-> (f1 :||: (f2 :||: f3))
-------------------------------------------------------------} 
              
       

associativity ((f1 :||: f2) :||: f3) new = 
    case new of
       (f4 :||: (f5 :||: f6)) -> if f1 == f4 && f2 == f5 && f3 == f6 
                                 then  Just FB { ruleName        = AssociativityOr
                                              , ruleApplied      = True
                                              , correctApplied   = True
                                              , removed          = ((f1 :||: f2) :||: f3)
                                              , introduced       = new
                                              , correct          = new
                                              }
                                 else Nothing                               
       otherwise              -> Nothing


associativity (f1 :||: (f2 :||: f3)) new = 
    case new of
       ((f4 :||: f5) :||: f6) -> if f1 == f4 && f2 == f5 && f3 == f6 
                                 then  Just FB { ruleName        = AssociativityOr
                                              , ruleApplied      = True
                                              , correctApplied   = True
                                              , removed          = ((f1 :||: f2) :||: f3)
                                              , introduced       = new
                                              , correct          = new
                                              }
                                  else Nothing                               
       otherwise              ->  Nothing

associativity ((f1 :&&: f2) :&&: f3) new = 
    case new of
       (f4 :&&: (f5 :&&: f6)) -> if f1 == f4 && f2 == f5 && f3 == f6 
                                 then  Just FB { ruleName        = AssociativityAnd
                                              , ruleApplied      = True
                                              , correctApplied   = True
                                              , removed          = ((f1 :&&: f2) :&&: f3)
                                              , introduced       = new
                                              , correct          = new
                                              }
                                 else Nothing                               
       otherwise              -> Nothing


associativity (f1 :&&: (f2 :&&: f3)) new = 
    case new of
       ((f4 :&&: f5) :&&: f6) -> if f1 == f4 && f2 == f5 && f3 == f6 
                                 then  Just FB { ruleName        = AssociativityAnd
                                              , ruleApplied      = True
                                              , correctApplied   = True
                                              , removed          = ((f1 :&&: f2) :&&: f3)
                                              , introduced       = new
                                              , correct          = new
                                              }
                                  else Nothing                               
       otherwise              ->  Nothing




associativity _ _             =   Nothing  


 
        
{-------------------------------------------------------------------------------------
 Function diff takes two proposition formulae (f and g) as input and
 calculates the difference between these formulae. Formula g is the
 rewritten one of function f. The result of function diff can be:
 - Nothing (of type Maybe), which mean there is no difference;
 - or the formula parts that are removed and/or introduced with
   respect to formula f (of type Maybe: Just(Formula, Formula). 

  This function must be expanded: what if T or F is added on top level(?).
--------------------------------------------------------------------------------------}


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
                                  else diff (f1, f2)
    ( _ , _)                   -> Just (f, g)



{--------------------------------------------------------------------
 Function allowedRuleApplied is called if (and only if) the submitted
 formula is semantically correct. Function allowedRuleApplied checks
 whether a knowable rule is applied, in which case the rule name is
 returned. If an unknowable rule or a sequence of rules is applied,
 value NoRuleDetected is returned.   
---------------------------------------------------------------------}


notAllowedRuleApplied ::  RuleName -> Bool
notAllowedRuleApplied r = r == NoRuleDetected 

allowedRuleApplied :: Formula -> Formula ->  RuleInfo

allowedRuleApplied old new =
    if isNothing diffOldNew 
    then RI { ruleInfo = NoDiff } 
    else
    if truefalseApplied 
    then RI { ruleInfo = TrueFalse }
    else
    if implApplied 
    then RI { ruleInfo = EliminateImplication }
    else
    if eqvApplied 
    then RI { ruleInfo = EliminateEquivalence }
    else    
    if deMoApplied 
    then RI { ruleInfo = DeMorgan }
    else 
    if negApplied  
    then RI { ruleInfo = RemoveDoubleNeg }
    else
    if distrApplied 
    then RI { ruleInfo = DistrAndoverOr }
    else
    if idempApplied
    then RI { ruleInfo = Idempotency }
    else
    if commApplied 
    then RI { ruleInfo = Commutativity }
    else
    if assocApplied 
    then RI { ruleInfo = Associativity }
    else RI { ruleInfo = NoRuleDetected }   
    
    where 
    
    diffOldNew = diff (old, new) 

    -- rem = removed, int = introduced
    rem    = if isNothing diffOldNew then undefined else fst (fromJust diffOldNew)
    int    = if isNothing diffOldNew then undefined else snd (fromJust diffOldNew)

    -- Rewrite analysis information    
    truefalseApplied = truefalseRuleApplied           rem int
    implApplied      = eliminateImplRuleApplied       rem int
    eqvApplied       = eliminateEqvRuleApplied        rem int
    deMoApplied      = deMorganRuleApplied            rem int
    negApplied       = doubleNegRuleApplied           rem int
    distrApplied     = distributeAndOverOrRuleApplied rem int
    commApplied      = commutativityRuleApplied       rem int
    assocApplied     = associativityRuleApplied       rem int
    idempApplied     = idempotencyRuleApplied         rem int
   

    truefalseRuleApplied (T :||: f) T         = True
    truefalseRuleApplied (f :||: T) T         = True
    truefalseRuleApplied (T :&&: f1) f2       = f1 == f2
    truefalseRuleApplied (f1 :&&: T) f2       = f1 == f2
    truefalseRuleApplied (F :||: f1) f2       = f1 == f2
    truefalseRuleApplied (f1 :||: F) f2       = f1 == f2
    truefalseRuleApplied (F :&&: f) F         = True
    truefalseRuleApplied (f :&&: F) F         = True
    truefalseRuleApplied (f1 :||: Not (f2)) T = f1 == f2
    truefalseRuleApplied (Not (f1) :||: f2) T = f1 == f2
    truefalseRuleApplied (f1 :&&: Not (f2)) F = f1 == f2
    truefalseRuleApplied (Not (f1) :&&: f2) F = f1 == f2
    truefalseRuleApplied (Not T) F            = True
    truefalseRuleApplied (Not F) T            = True
    truefalseRuleApplied _                  _ = False 

 
    eliminateImplRuleApplied (f1 :->: f2) ((Not f3) :||: f4) = f1 == f3 && f2 == f4
    eliminateImplRuleApplied ((Not f3) :||: f4) (f1 :->: f2) = f1 == f3 && f2 == f4
    eliminateImplRuleApplied _ _                             = False



    eliminateEqvRuleApplied  (f1 :<->: f2) ((f3 :&&: f4) :||: ((Not f5) :&&: (Not f6)))
                                                             = f1 == f3 && f2 == f4 && f1 == f5 && f2 == f6
    eliminateEqvRuleApplied  ((f3 :&&: f4) :||: ((Not f5) :&&: (Not f6))) (f1 :<->: f2) 
                                                             = f1 == f3 && f2 == f4 && f1 == f5 && f2 == f6
    eliminateEqvRuleApplied _ _                              = False



    deMorganRuleApplied (Not (f1 :&&: f2))       ((Not f3) :||: (Not f4)) = f1 == f3 && f2 == f4
    deMorganRuleApplied (Not (f1 :||: f2))       ((Not f3) :&&: (Not f4)) = f1 == f3 && f2 == f4
    deMorganRuleApplied ((Not f1) :||: (Not f2)) ((Not (f3 :&&: f4)))     = f1 == f3 && f2 == f4
    deMorganRuleApplied ((Not f1) :&&: (Not f2)) ((Not (f3 :||: f4)))     = f1 == f3 && f2 == f4
    deMorganRuleApplied _ _                                               = False



    doubleNegRuleApplied (Not (Not f1)) (f2) = f1 == f2
    doubleNegRuleApplied _ _                 = False
  

    distributeAndOverOrRuleApplied ((f4 :&&: f5) :||: (f6 :&&: f7)) (f1 :&&: (f2 :||: f3))
                                                              = f1 == f4 && f1 == f6 && f2 == f5 && f3 == f7 
    
    distributeAndOverOrRuleApplied f1 f2                      = (distributeAndOverOrLeft f1 f2) ||  (distributeAndOverOrRight f1 f2)

    
    distributeAndOverOrLeft  (f1 :&&: (f2 :||: f3)) ((f4 :&&: f5) :||: (f6 :&&: f7))
                                                              = f1 == f4 && f1 == f6 && f2 == f5 && f3 == f7 
    distributeAndOverOrLeft _ _                               = False
    
    distributeAndOverOrRight ((f2 :||: f3) :&&:f1 ) ((f4 :&&: f5) :||: (f6 :&&: f7)) 
                                                              =  (f1 == f5 && f2 == f4 && f1 == f7 && f3 == f6)
                                                              -- || (f4 == f1 && f5 == f2 && f6 == f1 && f7 == f3) 
                                                         
    distributeAndOverOrRight _ _                              = False  


    associativityRuleApplied  (f1 :||: (f2 :||: f3)) ((f4 :||: f5) :||: f6)  = f1 == f4 && f2 == f5 && f3 == f6 
    associativityRuleApplied ((f4 :||: f5) :||: f6)   (f1 :||: (f2 :||: f3)) = f1 == f4 && f2 == f5 && f3 == f6 
    associativityRuleApplied  (f1 :&&: (f2 :&&: f3)) ((f4 :&&: f5) :&&: f6)  = f1 == f4 && f2 == f5 && f3 == f6 
    associativityRuleApplied ((f4 :&&: f5) :&&: f6)   (f1 :&&: (f2 :&&: f3)) = f1 == f4 && f2 == f5 && f3 == f6 
    associativityRuleApplied _ _                                             = False


    
    commutativityRuleApplied (f1 :||: f2) (f3 :||: f4) = f1 == f4 && f2 == f3
    commutativityRuleApplied (f1 :&&: f2) (f3 :&&: f4) = f1 == f4 && f2 == f3
    commutativityRuleApplied _ _                       = False 



    idempotencyRuleApplied (f1 :&&: f2) (f3) = f1 == f2 && f2 == f3
    idempotencyRuleApplied (f1 :||: f2) (f3) = f1 == f2 && f2 == f3 
    idempotencyRuleApplied _ _               = False











 


