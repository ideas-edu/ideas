{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Domain.Logic.Rules where

import qualified Data.Set as S
import Domain.Logic.Formula
import Domain.Logic.Zipper
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
	     , buggyRuleCommImp, buggyRuleAssImp
             ]

-- local logic variables
x, y, z :: Logic
x:y:z:_ = map makeVarInt [0..]

ruleComplOr :: LogicRule
ruleComplOr = makeRuleList "ComplOr"
   [ (x :||: Not x)  |-  T
   , (Not x :||: x)  |-  T
   ]
   
ruleComplAnd :: LogicRule
ruleComplAnd = makeRuleList "ComplAnd"
   [ (x :&&: Not x)  |-  F
   , (Not x :&&: x)  |-  F
   ]

ruleDefImpl :: LogicRule
ruleDefImpl = makeRule "DefImpl" $
   (x :->: y)  |-  (Not x :||: y)
   
ruleDefEquiv :: LogicRule
ruleDefEquiv = makeRule "DefEquiv" $
   (x :<->: y)  |-  ((x :&&: y) :||: (Not x :&&: Not y))
   
ruleFalseInEquiv :: LogicRule
ruleFalseInEquiv = makeRuleList "FalseInEquiv"
   [ (F :<->: x)  |-  (Not x)
   , (x :<->: F)  |-  (Not x)
   ]
   
ruleTrueInEquiv :: LogicRule
ruleTrueInEquiv = makeRuleList "TrueInEquiv"
   [ (T :<->: x)  |-  x
   , (x :<->: T)  |-  x
   ]

ruleFalseInImpl :: LogicRule
ruleFalseInImpl = makeRuleList "FalseInImpl"
   [ (F :->: x)  |-  T
   , (x :->: F)  |- (Not x)
   ]
   
ruleTrueInImpl :: LogicRule
ruleTrueInImpl = makeRuleList "TrueInImpl"
   [  (T :->: x)  |-  x
   ,  (x :->: T)  |-  T
   ]
        
ruleFalseZeroOr :: LogicRule
ruleFalseZeroOr = makeRuleList "FalseZeroOr"
   [ (F :||: x)  |-  x
   , (x :||: F)  |-  x
   ]
  
ruleTrueZeroOr :: LogicRule
ruleTrueZeroOr = makeRuleList "TrueZeroOr"
   [ (T :||: x)  |-  T
   , (x :||: T)  |-  T
   ]

ruleTrueZeroAnd :: LogicRule
ruleTrueZeroAnd = makeRuleList "TrueZeroAnd"
   [ (T :&&: x)  |-  x
   , (x :&&: T)  |-  x
   ]

ruleFalseZeroAnd :: LogicRule
ruleFalseZeroAnd = makeRuleList "FalseZeroAnd"
   [ (F :&&: x)  |-  F
   , (x :&&: F)  |-  F
   ]

ruleDeMorganOr :: LogicRule
ruleDeMorganOr = makeRule "DeMorganOr" $
   (Not (x :||: y))  |-  (Not x :&&: Not y)

ruleDeMorganAnd :: LogicRule
ruleDeMorganAnd = makeRule "DeMorganAnd" $
   (Not (x :&&: y))  |-  (Not x :||: Not y)

ruleNotBoolConst :: LogicRule
ruleNotBoolConst = makeRuleList "NotBoolConst"
   [ (Not T)  |-  F
   , (Not F)  |-  T
   ]

ruleNotNot :: LogicRule
ruleNotNot = makeRule "NotNot" $ 
   (Not (Not x))  |-  x

ruleAndOverOr :: LogicRule
ruleAndOverOr = makeRuleList "AndOverOr"
   [ (x :&&: (y :||: z))  |-  ((x :&&: y) :||: (x :&&: z))
   , ((x :||: y) :&&: z)  |-  ((x :&&: z) :||: (y :&&: z))
   ]

ruleOrOverAnd :: LogicRule
ruleOrOverAnd = makeRuleList "OrOverAnd"
   [ (x :||: (y :&&: z))  |-  ((x :||: y) :&&: (x :||: z))
   , ((x :&&: y) :||: z)  |-  ((x :||: z) :&&: (y :||: z))
   ]
   
ruleIdempOr :: LogicRule
ruleIdempOr = makeRule "IdempOr" $
    (x :||: x)  |-  x
   
    
ruleIdempAnd :: LogicRule
ruleIdempAnd = makeRule "IdempAnd" $
    (x :&&: x)  |-  x
    
    
ruleAbsorpOr :: LogicRule
ruleAbsorpOr = makeRule "AbsorpOr" $
    (x :||: (x :&&: y))  |-  x
    
    
ruleAbsorpAnd :: LogicRule
ruleAbsorpAnd = makeRule "AbsorpAnd" $
    (x :&&: (x :||: y))  |-  x 
    
    
ruleCommOr :: LogicRule
ruleCommOr = makeRule "CommOr" $
    (x :||: y)  |-  (y :||: x) 
    
    
ruleCommAnd :: LogicRule
ruleCommAnd = makeRule "CommAnd" $
    (x :&&: y)  |-  (y :&&: x)
    
    
buggy :: LogicRule
buggy = makeSimpleRule "buggy" f
 where
   f (p :->: q) | p /= q = Just (q :->: p)
   f _          = Nothing


buggyRuleCommImp :: LogicRule
buggyRuleCommImp = makeRule "CommImp" $
    (x :->: y)  |-  (y :->: x) 

    
buggyRuleAssImp :: LogicRule
buggyRuleAssImp = makeRuleList "AssImp"
   [ (x :->: (y :->: z))  |-  ((x :->: y) :->: z)
   , ((x :->: y) :->: z)  |-  (x :->: (y :->: z))
   ]
    
buggyRuleIdemImp :: LogicRule
buggyRuleIdemImp = makeRule "IdemImp" $
    (x :->: x)  |-  x 
    
buggyRuleIdemEqui :: LogicRule
buggyRuleIdemEqui = makeRule "IdemEqui"  $
    (x :<->: x)  |-  x 
    
buggyRuleEquivElim :: LogicRule
buggyRuleEquivElim = makeRuleList "BuggyEquivElim"
    [ (x :<->: y) |- (x :&&: y) :||: Not (x :&&: y) 
    , (x :<->: y) |- (x :||: y) :&&: (Not x :||: Not y)
    , (x :<->: y) |- (x :&&: y) :||: (Not x :&&:  y)
    , (x :<->: y) |- (x :&&: y) :||: ( x :&&: Not y) 
    , (x :<->: y) |- (x :&&: y) :&&: (Not x :&&: Not y)
    ]
    
buggyRuleImplElim :: LogicRule
buggyRuleImplElim = makeRule "BuggyImplElim" $
    (x :->: y) |- Not (x :||: y) 
    
buggyRuleDeMorgan :: LogicRule
buggyRuleDeMorgan = makeRuleList "BuggyDeMorgan"
    [ (Not (x :&&: y)) |-  (Not x :||: y)
    , (Not (x :&&: y)) |-  (x :||: Not y)
    , (Not (x :&&: y)) |- (Not (Not x :||: Not y))
    , (Not (x :||: y)) |-  (Not x :&&: y)
    , (Not (x :||: y)) |-  (x :&&: Not y)
    , (Not (x :||: y)) |- (Not (Not x :&&: Not y)) --note the firstNot in both formulas!  
    ]
buggyRuleNotOverImpl :: LogicRule
buggyRuleNotOverImpl = makeRule "BuggyNotOverImpl" $
    (Not(x :->: y)) |- (Not x :->: Not y)   
    
buggyRuleParenth :: LogicRule
buggyRuleParenth = makeRuleList "BuggyParenth"
    [ (Not (x :&&: y)) |-  (Not x :&&: y)
    , (Not (x :||: y)) |-  (Not x :||: y)
    , (Not (x :<->: y) |- (Not(x :&&: y) :||: (Not x :&&: Not y))
    , (Not(Not p :&&: q)) |- (p :&&: q) 
    , (Not(Not p :||: q)) |- (p :||: q)
    , (Not(Not p :->: q)) |- (p :->: q)
    , (Not(Not p :<->: q)) |- (p :<->: q)
    ]
    
buggyRuleAssoc :: LogicRule
buggyRuleAssoc = makeRuleList "BuggyAssoc"
    [ (x :||: (y :&&: z)) |- ((x :||: y) :&&: z)
    , ((x :||: y) :&&: z) |- (x :||: (y :&&: z))
    , ((x :&&: y) :||: z) |- (x :&&: (y :||: z))
    , (x :&&: (y :||: z)) |- ((x :&&: y) :||: z)
    ]
=======
   
   






















































































>>>>>>> .theirs
