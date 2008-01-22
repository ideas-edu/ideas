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
            , ruleDivOne, ruleCommonDenom, ruleMulVar, ruleSubVar
            , ruleAssMul, ruleCommAdd, ruleCommMul, ruleDistMul
            , ruleAdd, ruleSub, ruleMul, ruleAddFrac, ruleSubFrac, ruleDiv
            ]

-- local frac variables
x, y, z :: Frac
x:y:z:_ = map makeVarInt [0..]

ruleUnitAdd :: FracRule
ruleUnitAdd = makeRuleList "UnitAdd"
   [ (x :+: Con 0)  |-  x
   , (Con 0 :+: x)  |-  x
   ]
   
ruleMulVar :: FracRule
ruleMulVar = makeRule "MulVar" $
   (x :+: x) |- x :*: Con 2

ruleSubZero :: FracRule
ruleSubZero = makeRule "SubZero" $
   (x :-: Con 0)  |-  x

ruleSubVar :: FracRule
ruleSubVar = makeRule "SubVar" $
   (x :-: x) |- Con 0

ruleMulZero :: FracRule
ruleMulZero = makeRuleList "MulZero"
   [ (x :*: Con 0)  |-  Con 0
   , (Con 0 :*: x)  |-  Con 0
   ]

ruleUnitMul :: FracRule
ruleUnitMul = makeRuleList "UnitMul"
   [ (x :*: Con 1)  |-  x
   , (Con 1 :*: x)  |-  x
   ]

ruleCommonDenom :: FracRule
ruleCommonDenom = makeSimpleRule "CommonDenom" f
 where
  f (Con x :/: Con y :+: Con v :/: Con w) = return $ Con (x*w) :/: Con (y*w) 
                                                     :+: Con (v*y) :/: Con (y*w)
  f (Con x :/: Con y :-: Con v :/: Con w) = return $ Con (x*w) :/: Con (y*w) 
                                                     :-: Con (v*y) :/: Con (y*w)
  f (Con x :+: Con v :/: Con w) = return $ Con (x*w) :/: Con (w) :+: Con (v) :/: Con (w)
  f (Con x :/: Con y :+: Con v) = return $ Con (x) :/: Con (y) :+: Con (v*y) :/: Con (y)
  f _                 = Nothing

ruleDivOne :: FracRule
ruleDivOne = makeRule "DivOne" $
   (x :/: Con 1)  |-  x

ruleDivZero :: FracRule
ruleDivZero = makeRule "DivZero" $
   (Con 0 :/: x)  |-  Con 0

ruleDivReciprocal :: FracRule
ruleDivReciprocal = makeRule "DivReciprocal" $
   (x :/: (y :/: z)) |- ((x :*: z) :/: y)

ruleDivSame :: FracRule
ruleDivSame = makeRule "DivSame" $
   (x :/: x) |- Con 1

ruleAdd :: FracRule
ruleAdd = makeSimpleRule "Add" f
 where
   f (Con a :+: Con b) = return $ Con (a+b)
   f _                 = Nothing

ruleAddFrac :: FracRule
ruleAddFrac = makeSimpleRule "AddFrac" f
 where
   f (Con x :/: Con y :+: Con v :/: Con w) | y==w = return $ Con (x+v) :/: Con w
                                           | otherwise = Nothing
   f _                 = Nothing

ruleSub :: FracRule
ruleSub = makeSimpleRule "Sub" f
 where
   f (Con a :-: Con b) = return $ Con (a-b)
   f _                 = Nothing

ruleSubFrac :: FracRule
ruleSubFrac = makeSimpleRule "SubFrac" f
 where
   f (Con x :/: Con y :-: Con v :/: Con w) | y==w = return $ Con (x-v) :/: Con w
                                           | otherwise = Nothing
   f _                 = Nothing

ruleMul :: FracRule
ruleMul = makeSimpleRule "Mul" f
 where
   f (Con a :*: Con b) = return $ Con (a*b)
   f ((Con x :/: Con y) :*: (Con v :/: Con w)) = return $ Con (x*v) :/: Con (y*w)
   f _                 = Nothing

ruleDiv :: FracRule
ruleDiv = makeSimpleRule "Div" f
 where
   f ((Con x :/: Con y) :/: (Con v :/: Con w)) = return $ Con (x*w) :/: Con (y*v)
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
   [ (x :*: y :+: x :*: z) |- x :*: (y :+: z)
   , (x :*: y :+: z :*: x) |- x :*: (y :+: z)
   , (y :*: x :+: x :*: z) |- x :*: (y :+: z)
   , (y :*: x :+: z :*: x) |- x :*: (y :+: z)

   , (x :/: y :+: x :*: z) |- x :*: (Con 1 :/: y :+: z)
   , (x :/: y :+: z :*: x) |- x :*: (Con 1 :/: y :+: z)

   , (x :*: y :+: x :/: z) |- x :*: (y :+: Con 1 :/: z)

   , (x :/: y :+: x :/: z) |- x :*: (Con 1 :/: y :+: Con 1 :/: z)
--
   , (x :*: y :-: x :*: z) |- x :*: (y :-: z)
   , (x :*: y :-: z :*: x) |- x :*: (y :-: z)
   , (y :*: x :-: x :*: z) |- x :*: (y :-: z)
   , (y :*: x :-: z :*: x) |- x :*: (y :-: z)

   , (x :/: y :-: x :*: z) |- x :*: (Con 1 :/: y :-: z)
   , (x :/: y :-: z :*: x) |- x :*: (Con 1 :/: y :-: z)

   , (x :*: y :-: x :/: z) |- x :*: (y :-: Con 1 :/: z)

   , (x :/: y :-: x :/: z) |- x :*: (Con 1 :/: y :-: Con 1 :/: z)
   ]
