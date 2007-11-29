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
module Logic.Rules where

import qualified Data.Set as S
import Logic.Domain
import Logic.Zipper
import Transformation
import Unification

type LogicRule = Rule Logic  

logicRules :: [LogicRule]
logicRules = [ ruleFalseZeroOr, ruleTrueZeroOr, ruleTrueZeroAnd, ruleFalseZeroAnd, ruleDeMorganOr, ruleDeMorganAnd
             , ruleNotBoolConst, ruleNotNot, ruleAndOverOr, ruleOrOverAnd
             , ruleDefImpl, ruleDefEquiv
             , ruleFalseInEquiv, ruleTrueInEquiv, ruleFalseInImpl, ruleTrueInImpl
	     , ruleComplOr, ruleComplAnd
             ]

logicRuleInContext :: Rule Logic -> Rule LogicInContext
logicRuleInContext r = makeRule (name r) (maybeLoc . fmap (apply r))

-- local logic variables
x, y, z :: Logic
x:y:z:_ = map makeVarInt [0..]

ruleComplOr :: LogicRule
ruleComplOr = makePatternRule "ComplOr"
   $   (x :||: Not x)  |-  T
   ++  (Not x :||: x)  |-  T

ruleComplAnd :: LogicRule
ruleComplAnd = makePatternRule "ComplAnd"
   $  (x :&&: Not x)  |-  F
   ++ (Not x :&&: x)  |-  F

ruleDefImpl :: LogicRule
ruleDefImpl = makePatternRule "DefImpl" $
   (x :->: y)  |-  (Not x :||: y)
   
ruleDefEquiv :: LogicRule
ruleDefEquiv = makePatternRule "DefEquiv" $
   (x :<->: y)  |-  ((x :&&: y) :||: (Not x :&&: Not y))
   
ruleFalseInEquiv :: LogicRule
ruleFalseInEquiv = makePatternRule "FalseInEquiv"
   $  (F :<->: x)  |-  (Not x)
   ++ (x :<->: F)  |-  (Not x)
   
ruleTrueInEquiv :: LogicRule
ruleTrueInEquiv = makePatternRule "TrueInEquiv"
   $  (T :<->: x)  |-  x
   ++ (x :<->: T)  |-  x

ruleFalseInImpl :: LogicRule
ruleFalseInImpl = makePatternRule "FalseInImpl"
   $  (F :->: x)  |-  T
   ++ (x :->: F)  |- (Not x)
   
ruleTrueInImpl :: LogicRule
ruleTrueInImpl = makePatternRule "TrueInImpl"
   $   (T :->: x)  |-  x
   ++  (x :->: T)  |-  T
        
ruleFalseZeroOr :: LogicRule
ruleFalseZeroOr = makePatternRule "FalseZeroOr"
   $  (F :||: x)  |-  x
   ++ (x :||: F)  |-  x
  
ruleTrueZeroOr :: LogicRule
ruleTrueZeroOr = makePatternRule "TrueZeroOr"
   $  (T :||: x)  |-  T
   ++ (x :||: T)  |-  T

ruleTrueZeroAnd :: LogicRule
ruleTrueZeroAnd = makePatternRule "TrueZeroAnd"
   $  (T :&&: x)  |-  x
   ++ (x :&&: T)  |-  x

ruleFalseZeroAnd :: LogicRule
ruleFalseZeroAnd = makePatternRule "FalseZeroAnd"
   $  (F :&&: x)  |-  F
   ++ (x :&&: F)  |-  F

ruleDeMorganOr :: LogicRule
ruleDeMorganOr = makePatternRule "DeMorganOr" $
   (Not (x :||: y))  |-  (Not x :&&: Not y)

ruleDeMorganAnd :: LogicRule
ruleDeMorganAnd = makePatternRule "DeMorganAnd" $
   (Not (x :&&: y))  |-  (Not x :||: Not y)

ruleNotBoolConst :: LogicRule
ruleNotBoolConst = makePatternRule "NotBoolConst"
   $  (Not T)  |-  F
   ++ (Not F)  |-  T

ruleNotNot :: LogicRule
ruleNotNot = makePatternRule "NotNot" $ 
   (Not (Not x))  |-  x

ruleAndOverOr :: LogicRule
ruleAndOverOr = makePatternRule "AndOverOr"
   $  (x :&&: (y :||: z))  |-  ((x :&&: y) :||: (x :&&: z))
   ++ ((x :||: y) :&&: z)  |-  ((x :&&: z) :||: (y :&&: z))

ruleOrOverAnd :: LogicRule
ruleOrOverAnd = makePatternRule "OrOverAnd"
   $  (x :||: (y :&&: z))  |-  ((x :||: y) :&&: (x :||: z))
   ++ ((x :&&: y) :||: z)  |-  ((x :||: z) :&&: (y :||: z))