{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Domain.Fraction.Rules where

import qualified Data.Set as S
import Domain.Fraction.Frac
import Domain.Fraction.Zipper
import Common.Transformation
import Common.Unification

type FracRule = Rule Frac

fracRules :: [FracRule]
fracRules = [ ruleUnitAdd, ruleSubZero, ruleMulZero, ruleUnitMul
            , ruleDivOne, ruleDivZero, ruleDivReciprocal
            , ruleAdd, ruleSub, ruleDiv, ruleMul, ruleAssAdd
            , ruleAssMul, ruleCommAdd, ruleCommMul, ruleDistMul            
            ]

-- local frac variables
x, y, z :: Frac
x:y:z:_ = map makeVarInt [0..]

a, b :: Rational
a = 2
b = 2

ruleUnitAdd :: FracRule
ruleUnitAdd = makeRuleList "UnitAdd"
   [ (x :+: Lit 0)  |-  x
   , (Lit 0 :+: x)  |-  x
   ]
   
ruleSubZero :: FracRule
ruleSubZero = makeRule "SubZero" $
   (x :-: Lit 0)  |-  x

ruleMulZero :: FracRule
ruleMulZero = makeRuleList "MulZero"
   [ (x :*: Lit 0)  |-  Lit 0
   , (Lit 0 :*: x)  |-  Lit 0
   ]

ruleUnitMul :: FracRule
ruleUnitMul = makeRuleList "MulOne"
   [ (x :*: Lit 1)  |-  x
   , (Lit 1 :*: x)  |-  x
   ]

ruleDivOne :: FracRule
ruleDivOne = makeRule "DivOne" $
   (x :/: Lit 1)  |-  x

ruleDivZero :: FracRule
ruleDivZero = makeRule "DivZero" $
   (Lit 0 :/: x)  |-  Lit 0
     where 

ruleDivReciprocal :: FracRule
ruleDivReciprocal = makeRule "DivReciprocal" $
   (x :/: (y :/: z)) |- ((x :*: z) :/: y)

ruleAdd :: FracRule
ruleAdd = makeRule "Add" $
   (Lit a :+: Lit b)  |-  Lit (a+b)
 
ruleSub :: FracRule
ruleSub = makeRule "Sub" $
   (Lit a :-: Lit b)  |-  Lit (a-b)

ruleDiv :: FracRule
ruleDiv = makeRule "Div" $
   (Lit a :/: Lit b)  |-  Lit (a/b) --check y non zero

ruleMul :: FracRule
ruleMul = makeRule "Mul" $
   (Lit a :*: Lit b)  |-  Lit (a*b)
  
-- todo:  distribution, negate, 

ruleAssAdd :: FracRule
ruleAssAdd = makeRule "AssAdd" $
   (x :+: (y :+: z)) |- ((x :+: y) :+: z)

ruleAssMul :: FracRule
ruleAssMul = makeRule "AssMul" $
   x :*: (y :*: z) |- (x :*: y) :*: z

ruleCommAdd :: FracRule
ruleCommAdd = makeRule "CommAdd" $
   (x :+: y) |- (y :+: x)

ruleCommMul :: FracRule
ruleCommMul = makeRule "CommMul" $
   x :*: y |- y :*: x

ruleDistMul :: FracRule
ruleDistMul = makeRuleList "DistMul" 
   [ x :*: (y :+: z) |- (x :*: y :+: x :*: z)
   , (x :+: y) :*: z |- (x :*: z :+: y :*: z)
   , x :*: (y :-: z) |- (x :*: y :-: x :*: z)
   , (x :-: y) :*: z |- (x :*: z :-: y :*: z)
   ]
