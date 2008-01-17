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
import Ratio

type FracRule = Rule Frac

fracRules :: [FracRule]
fracRules = [ ruleDivZero, ruleAssAdd, ruleDivReciprocal
            , ruleUnitAdd, ruleSubZero, ruleMulZero, ruleUnitMul
            , ruleDivOne, ruleCommonDenom
            , ruleAssMul, ruleCommAdd, ruleCommMul, ruleDistMul
            , ruleAdd, ruleSub, ruleMul, ruleDiv
            ]

-- local frac variables
x, y, z :: Frac
x:y:z:_ = map makeVarInt [0..]

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
ruleUnitMul = makeRuleList "UnitMul"
   [ (x :*: Lit 1)  |-  x
   , (Lit 1 :*: x)  |-  x
   ]

ruleCommonDenom :: FracRule
ruleCommonDenom = makeSimpleRule "CommonDenom" f
 where
   f (Lit a :/: Lit b) = return $ Lit ((na*db)%(da*db)) :/: Lit ((nb*da)%(da*db))
     where
       na = numerator a 
       nb = numerator b
       da = denominator a
       db = denominator b
   f _                         = Nothing


ruleDivOne :: FracRule
ruleDivOne = makeRule "DivOne" $
   (x :/: Lit 1)  |-  x

ruleDivZero :: FracRule
ruleDivZero = makeRule "DivZero" $
   (Lit 0 :/: x)  |-  Lit 0

ruleDivReciprocal :: FracRule
ruleDivReciprocal = makeRule "DivReciprocal" $
   (x :/: (y :/: z)) |- ((x :*: z) :/: y)

ruleAdd :: FracRule
ruleAdd = makeSimpleRule "Add" f
 where
   f (Lit a :+: Lit b) = return $ Lit (a+b)
   f _                 = Nothing
 
ruleSub :: FracRule
ruleSub = makeSimpleRule "Sub" f
 where
   f (Lit a :-: Lit b) = return $ Lit (a-b)
   f _                 = Nothing

ruleDiv :: FracRule
ruleDiv = makeSimpleRule "Div" f
 where
   f (Lit a :/: Lit b) = return $ Lit (a/b)  --check non zero
   f _                 = Nothing

ruleMul :: FracRule
ruleMul = makeSimpleRule "Mul" f
 where
   f (Lit a :*: Lit b) = return $ Lit (a*b)
   f _                 = Nothing

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
