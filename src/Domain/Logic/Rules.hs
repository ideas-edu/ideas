{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (...add description...)
--
-----------------------------------------------------------------------------
module Domain.Logic.Rules where

import qualified Data.Set as S
import Domain.Logic.Formula
import Common.Transformation
import Common.Unification

type LogicRule = Rule Logic  

logicRules :: [LogicRule]
logicRules = [ ruleFalseZeroOr, ruleTrueZeroOr, ruleTrueZeroAnd, ruleFalseZeroAnd, ruleDeMorganOr, ruleDeMorganAnd
             , ruleNotBoolConst, ruleNotNot, ruleAndOverOr, ruleOrOverAnd
             , ruleDefImpl, ruleDefEquiv
             , ruleFalseInEquiv, ruleTrueInEquiv, ruleFalseInImpl, ruleTrueInImpl
	     , ruleComplOr, ruleComplAnd
	     , ruleIdempOr, ruleIdempAnd
	     , ruleAbsorpOr, ruleAbsorpAnd
	     , ruleCommOr, ruleCommAnd
             ]

logicBuggyRules :: [LogicRule]
logicBuggyRules = [ buggyRuleCommImp, buggyRuleAssImp
                  ]

-- local logic variables
x, y, z :: Logic
x:y:z:_ = metaVars

-- make rule associative
makeRuleA :: String -> Transformation Logic -> LogicRule
makeRuleA s = makeRuleList s . transA

makeRuleListA :: String -> [Transformation Logic] -> LogicRule
makeRuleListA s = makeRuleList s . concatMap transA

transA :: Transformation Logic -> [Transformation Logic]
transA t = case isPatternPair t of
              Nothing -> [t]
              Just qp -> 
                 case instantiateWith substitutePair 0 qp of
                    ((lhs@(_ :||: _), rhs), n) -> [t, (lhs :||: metaVar n) |- (rhs :||: metaVar n)]
                    ((lhs@(_ :&&: _), rhs), n) -> [t, (lhs :&&: metaVar n) |- (rhs :&&: metaVar n)]
                    _ -> [t]

-----------------------------------------------------------------------------


ruleComplOr :: LogicRule
ruleComplOr = makeRuleListA "ComplOr"
   [ (x :||: Not x)  |-  T
   , (Not x :||: x)  |-  T
   ]
   
ruleComplAnd :: LogicRule
ruleComplAnd = makeRuleListA "ComplAnd"
   [ (x :&&: Not x)  |-  F
   , (Not x :&&: x)  |-  F
   ]

ruleDefImpl :: LogicRule
ruleDefImpl = makeRuleA "DefImpl" $
   (x :->: y)  |-  (Not x :||: y)
   
ruleDefEquiv :: LogicRule
ruleDefEquiv = makeRuleA "DefEquiv" $
   (x :<->: y)  |-  ((x :&&: y) :||: (Not x :&&: Not y))
   
ruleFalseInEquiv :: LogicRule
ruleFalseInEquiv = makeRuleListA "FalseInEquiv"
   [ (F :<->: x)  |-  (Not x)
   , (x :<->: F)  |-  (Not x)
   ]
   
ruleTrueInEquiv :: LogicRule
ruleTrueInEquiv = makeRuleListA "TrueInEquiv"
   [ (T :<->: x)  |-  x
   , (x :<->: T)  |-  x
   ]

ruleFalseInImpl :: LogicRule
ruleFalseInImpl = makeRuleListA "FalseInImpl"
   [ (F :->: x)  |-  T
   , (x :->: F)  |- (Not x)
   ]
   
ruleTrueInImpl :: LogicRule
ruleTrueInImpl = makeRuleListA "TrueInImpl"
   [  (T :->: x)  |-  x
   ,  (x :->: T)  |-  T
   ]
        
ruleFalseZeroOr :: LogicRule
ruleFalseZeroOr = makeRuleListA "FalseZeroOr"
   [ (F :||: x)  |-  x
   , (x :||: F)  |-  x
   ]
  
ruleTrueZeroOr :: LogicRule
ruleTrueZeroOr = makeRuleListA "TrueZeroOr"
   [ (T :||: x)  |-  T
   , (x :||: T)  |-  T
   ]

ruleTrueZeroAnd :: LogicRule
ruleTrueZeroAnd = makeRuleListA "TrueZeroAnd"
   [ (T :&&: x)  |-  x
   , (x :&&: T)  |-  x
   ] 

ruleFalseZeroAnd :: LogicRule
ruleFalseZeroAnd = makeRuleListA "FalseZeroAnd"
   [ (F :&&: x)  |-  F
   , (x :&&: F)  |-  F
   ]

ruleDeMorganOr :: LogicRule
ruleDeMorganOr = makeRuleA "DeMorganOr" $
   (Not (x :||: y))  |-  (Not x :&&: Not y)

ruleDeMorganAnd :: LogicRule
ruleDeMorganAnd = makeRuleA "DeMorganAnd" $
   (Not (x :&&: y))  |-  (Not x :||: Not y)

ruleNotBoolConst :: LogicRule
ruleNotBoolConst = makeRuleListA "NotBoolConst"
   [ (Not T)  |-  F
   , (Not F)  |-  T
   ]

ruleNotNot :: LogicRule
ruleNotNot = makeRuleA "NotNot" $ 
   (Not (Not x))  |-  x

ruleAndOverOr :: LogicRule
ruleAndOverOr = makeRuleListA "AndOverOr"
   [ (x :&&: (y :||: z))  |-  ((x :&&: y) :||: (x :&&: z))
   , ((x :||: y) :&&: z)  |-  ((x :&&: z) :||: (y :&&: z))
   ]

ruleOrOverAnd :: LogicRule
ruleOrOverAnd = makeRuleListA "OrOverAnd"
   [ (x :||: (y :&&: z))  |-  ((x :||: y) :&&: (x :||: z))
   , ((x :&&: y) :||: z)  |-  ((x :||: z) :&&: (y :||: z))
   ]
   
ruleIdempOr :: LogicRule
ruleIdempOr = makeRuleA "IdempOr" $
    (x :||: x)  |-  x
   
ruleIdempAnd :: LogicRule
ruleIdempAnd = makeRuleA "IdempAnd" $
    (x :&&: x)  |-  x
    
    
ruleAbsorpOr :: LogicRule
ruleAbsorpOr = makeRuleA "AbsorpOr" $
    (x :||: (x :&&: y))  |-  x
    
    
ruleAbsorpAnd :: LogicRule
ruleAbsorpAnd = makeRuleA "AbsorpAnd" $
    (x :&&: (x :||: y))  |-  x 
    
    
ruleCommOr :: LogicRule
ruleCommOr = makeRuleA "CommOr" $
    (x :||: y)  |-  (y :||: x) 
    
    
ruleCommAnd :: LogicRule
ruleCommAnd = makeRuleA "CommAnd" $
    (x :&&: y)  |-  (y :&&: x)
    
    

-- Buggy rules:

buggyRuleCommImp :: LogicRule
buggyRuleCommImp = buggyRule $ makeRuleA "CommImp" $
    (x :->: y)  |-  (y :->: x) --this does not hold: T->T => T->x

    
buggyRuleAssImp :: LogicRule
buggyRuleAssImp = buggyRule $ makeRuleListA "AssImp"
   [ (x :->: (y :->: z))  |-  ((x :->: y) :->: z)
   , ((x :->: y) :->: z)  |-  (x :->: (y :->: z))
   ]
    
buggyRuleIdemImp :: LogicRule
buggyRuleIdemImp = buggyRule $ makeRuleA "IdemImp" $
    (x :->: x)  |-  x 
    
buggyRuleIdemEqui :: LogicRule
buggyRuleIdemEqui = buggyRule $ makeRuleA "IdemEqui"  $
    (x :<->: x)  |-  x 
    
buggyRuleEquivElim :: LogicRule
buggyRuleEquivElim = buggyRule $ makeRuleListA "BuggyEquivElim"
    [ (x :<->: y) |- ((x :&&: y) :||: Not (x :&&: y))
    , (x :<->: y) |- ((x :||: y) :&&: (Not x :||: Not y))
    , (x :<->: y) |- ((x :&&: y) :||: (Not x :&&:  y))
    , (x :<->: y) |- ((x :&&: y) :||: ( x :&&: Not y))
    , (x :<->: y) |- ((x :&&: y) :&&: (Not x :&&: Not y))
    ]
    
buggyRuleImplElim :: LogicRule
buggyRuleImplElim = buggyRule $ makeRuleA "BuggyImplElim" $
    (x :->: y) |- Not (x :||: y) 
    
buggyRuleDeMorgan :: LogicRule
buggyRuleDeMorgan = buggyRule $ makeRuleListA "BuggyDeMorgan"
    [ (Not (x :&&: y)) |-  (Not x :||: y)
    , (Not (x :&&: y)) |-  (x :||: Not y)
    , (Not (x :&&: y)) |- (Not (Not x :||: Not y))
    , (Not (x :||: y)) |-  (Not x :&&: y)
    , (Not (x :||: y)) |-  (x :&&: Not y)
    , (Not (x :||: y)) |- (Not (Not x :&&: Not y)) --note the firstNot in both formulas!  
    ]
buggyRuleNotOverImpl :: LogicRule
buggyRuleNotOverImpl = buggyRule $ makeRuleA "BuggyNotOverImpl" $
    (Not(x :->: y)) |- (Not x :->: Not y)   
    
buggyRuleParenth :: LogicRule
buggyRuleParenth = buggyRule $ makeRuleListA "BuggyParenth"
    [ (Not (x :&&: y)) |-  (Not x :&&: y)
    , (Not (x :||: y)) |-  (Not x :||: y)
    , (Not (x :<->: y)) |- (Not(x :&&: y) :||: (Not x :&&: Not y))
    , (Not(Not x :&&: y)) |- (x :&&: y) 
    , (Not(Not x :||: y)) |- (x :||: y)
    , (Not(Not x :->: y)) |- (x :->: y)
    , (Not(Not x :<->: y)) |- (x :<->: y)
    ]
    
buggyRuleAssoc :: LogicRule
buggyRuleAssoc = buggyRule $ makeRuleListA "BuggyAssoc"
    [ (x :||: (y :&&: z)) |- ((x :||: y) :&&: z)
    , ((x :||: y) :&&: z) |- (x :||: (y :&&: z))
    , ((x :&&: y) :||: z) |- (x :&&: (y :||: z))
    , (x :&&: (y :||: z)) |- ((x :&&: y) :||: z)
    ]

