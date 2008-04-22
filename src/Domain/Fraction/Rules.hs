{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
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
import Common.Transformation
import Common.Unification
import Ratio

type FracRule = Rule Frac

fracRules :: [FracRule]
fracRules = [ ruleDivZero, ruleAssAdd, ruleGCD, ruleNeg
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
   [ (x :*: Con 1)     |-  x
   , (Con 1 :*: x)     |-  x
   , (Con (-1) :*: x)  |-  negate x
   ]

ruleCommonDenom :: FracRule
ruleCommonDenom = makeSimpleRule "CommonDenom" f
 where
  f (Con x :/: Con y :+: Con v :/: Con w) | y/=w = return $ Con (x*w) :/: Con (y*w) 
                                                            :+: Con (v*y) :/: Con (y*w)
                                          | otherwise = Nothing
  f (Con x :/: Con y :-: Con v :/: Con w) | y/=w = return $ Con (x*w) :/: Con (y*w) 
                                                            :-: Con (v*y) :/: Con (y*w)
                                          | otherwise = Nothing
  f (Con x :+: Con v :/: Con w) = return $ Con (x*w) :/: Con w :+: Con v :/: Con w
  f (Con x :/: Con y :+: Con v) = return $ Con x :/: Con y :+: Con (v*y) :/: Con y
  f (Con x :-: Con v :/: Con w) = return $ Con (x*w) :/: Con w :-: Con v :/: Con w
  f (Con x :/: Con y :-: Con v) = return $ Con x :/: Con y :-: Con (v*y) :/: Con y
  f _                 = Nothing

ruleGCD :: FracRule
ruleGCD = makeSimpleRule "GCD" f
 where
  f (Con x :/: Con y) | a==1      = Nothing
                      | otherwise = return $ Con (x `div` a) :/: Con (y `div` a)
    where
      a = gcd x y
  f _ = Nothing

ruleDivOne :: FracRule
ruleDivOne = makeRuleList "DivOne" 
   [ (x :/: Con 1)     |-  x
   , (x :/: Con (-1))  |-  Neg x
   ]

ruleDivZero :: FracRule
ruleDivZero = makeRule "DivZero" $
   (Con 0 :/: x)  |-  Con 0

ruleDivSame :: FracRule
ruleDivSame = makeRule "DivSame" $
   (x :/: x) |- Con 1

ruleAdd :: FracRule
ruleAdd = makeSimpleRule "Add" f
 where
   f (Con x :+: Con y) = return $ Con (x+y)
   f (x :+: Neg y)     = return $ x :-: y                         
   f _                 = Nothing

ruleAddFrac :: FracRule
ruleAddFrac = makeSimpleRule "AddFrac" f
 where
   f (x :/: y :+: v :/: w) | y==w = return $ (x+v) :/: w
                                           | otherwise = Nothing
   f _                 = Nothing

ruleSub :: FracRule
ruleSub = makeSimpleRule "Sub" f
 where
   f (Con x :-: Con y) = return $ Con (x-y)
   f (x :-: Neg y)     = return $ x :+: y
   f _                 = Nothing

ruleSubFrac :: FracRule
ruleSubFrac = makeSimpleRule "SubFrac" f
 where
   f (x :/: y :-: v :/: w) | y==w = return $ (x-v) :/: w
                                           | otherwise = Nothing
   f _                 = Nothing

ruleMul :: FracRule
ruleMul = makeSimpleRule "Mul" f
 where
   f (Con a :*: Con b) = return $ Con (a*b)
   f ((x :/: y) :*: (v :/: w)) = return $ (x :*: v) :/: (y :*: w)
   f ((x :/: y) :*: v) = return $ (x :*: v) :/: y
   f (v :*: (x :/: y)) = return $ (x :*: v) :/: y
   f (Neg x :*: Neg y) = return $ x :*: y
   f _                 = Nothing

ruleDiv :: FracRule
ruleDiv = makeSimpleRule "Div" f
 where
   f ((x :/: y) :/: (v :/: w)) = return $ (x :/: y) :*: (w :/: v)
   f ((x :/: y) :/: v) = return $ x :/: (y :*: v)
   f (x :/: (y :/: v)) = return $ (x :*: v) :/: y
   f (Neg x :/: Neg y) = return $ x :/: y
   f (Con x :/: Con y) | x<0 && y<0 = return $ Con (negate x) :/: Con (negate y)
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

ruleNeg :: FracRule
ruleNeg = makeSimpleRule "Neg" f
  where
    f (Neg (Neg x)) = return x
    f (Neg x)       = return $ foldFrac ((\x->Neg $ Var x),(\x->Con $ negate x),(:*:),(:/:),(:+:),(:-:), id) x
    f _             = Nothing

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
