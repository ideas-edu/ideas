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

import Domain.Fraction.Frac
import Common.Transformation
import Common.Unification

type FracRule = Rule Frac

fracRules :: [FracRule]
fracRules = [ ruleMulZero, ruleUnitMul, ruleMul, ruleMulFrac
            , ruleDivOne, ruleDivZero, ruleDivFrac, ruleDivSame
            , ruleUnitAdd, ruleAdd, ruleAddFrac
            , ruleSubZero, ruleSub, ruleSubVar, ruleSubFrac
            , ruleNeg, rulePushNeg, ruleNegToCon 
            , ruleCommonDenom, ruleGCD
            , ruleAssAdd, ruleAssMul
            , ruleCommAdd, ruleCommMul
            , ruleDistMul ]

-- local frac variables
v, w, x, y, z :: Frac
v:w:x:y:z:_ = metaVars


-- | Multiplication rules
ruleMulZero :: FracRule
ruleMulZero = makeRuleList "MulZero"
   [ (x :*: Con 0) |- Con 0
   , (Con 0 :*: x) |- Con 0
   ]

ruleUnitMul :: FracRule
ruleUnitMul = makeRuleList "UnitMul"
   [ (x :*: Con 1) |- x
   , (Con 1 :*: x) |- x
   ]

ruleMul :: FracRule
ruleMul = makeSimpleRule "Mul" f
 where
   f (Con a :*: Con b) = return $ Con (a*b)
   f _                 = Nothing

ruleMulFrac :: FracRule
ruleMulFrac = makeRuleList "MulFrac"
   [ (x :/: y) :*: (v :/: w) |-  (x :*: v) :/: (y :*: w)
   , (x :/: y) :*: v         |-  (x :*: v) :/: y
   , v :*: (x :/: y)         |-  (x :*: v) :/: y
   ]

ruleAssMul :: FracRule
ruleAssMul = makeRuleList "AssMul"
   [ x :*: (y :*: z) |- (x :*: y) :*: z
   , (x :*: y) :*: z |- x :*: (y :*: z)
   ]

ruleCommMul :: FracRule
ruleCommMul = makeRule "CommMul" $
   x :*: y |- y :*: x

-- also other way around?
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

-- | Division rules
ruleDivOne :: FracRule
ruleDivOne = makeRule "DivOne" $
   (x :/: Con 1) |- x   

ruleDivZero :: FracRule
ruleDivZero = makeRule "DivZero" $
   (Con 0 :/: x)  |-  Con 0

ruleDivFrac :: FracRule
ruleDivFrac = makeSimpleRule "Div" f
 where
   f ((x :/: y) :/: (v :/: w)) = return $ (x :/: y) :*: (w :/: v)
   f ((x :/: y) :/: v)         = return $ x :/: (y :*: v)
   f (x :/: (y :/: v))         = return $ (x :*: v) :/: y
   f _                 = Nothing

ruleDivSame :: FracRule
ruleDivSame = makeRule "DivSame" $
   (x :/: x) |- Con 1

-- | Addition rules
ruleUnitAdd :: FracRule
ruleUnitAdd = makeRuleList "UnitAdd"
   [ (x :+: Con 0)  |-  x
   , (Con 0 :+: x)  |-  x
   ]

ruleAdd :: FracRule
ruleAdd = makeSimpleRule "Add" f
 where 
   f (Con x :+: Con y) = return $ Con (x+y)
   f (x :+: y)         | x==y = return $ x :*: Con 2
                       | otherwise = Nothing
   f _                 = Nothing

ruleAddFrac :: FracRule
ruleAddFrac = makeSimpleRule "AddFrac" f
 where
   f (x :/: y :+: v :/: w) | y==w = return $ (x+v) :/: w
                           | otherwise = Nothing
   f _                     = Nothing

ruleAssAdd :: FracRule
ruleAssAdd = makeRuleList "AssAdd" 
   [ (x :+: (y :+: z)) |- ((x :+: y) :+: z)
   , ((x :+: y) :+: z) |- (x :+: (y :+: z))
   ]

ruleCommAdd :: FracRule
ruleCommAdd = makeRule "CommAdd" $
   (x :+: y) |- (y :+: x)

-- | Substraction rules
ruleSubZero :: FracRule
ruleSubZero = makeRule "SubZero" $
   (x :-: Con 0) |-  x

ruleSubVar :: FracRule
ruleSubVar = makeRule "SubVar" $
   (x :-: x) |- Con 0

ruleSub :: FracRule
ruleSub = makeSimpleRule "Sub" f
 where
   f (Con x :-: Con y) = return $ Con (x-y)
   f _                 = Nothing

ruleSubFrac :: FracRule
ruleSubFrac = makeSimpleRule "SubFrac" f
 where
   f (x :/: y :-: v :/: w) | y==w = return $ (x-v) :/: w
                           | otherwise = Nothing
   f _                 = Nothing

-- | Negation rules
ruleNeg :: FracRule
ruleNeg = makeSimpleRule "Neg" f
  where
    f (Con (-1) :*: x)  = return $ Neg x
    f (Neg x :*: Neg y) = return $ x :*: y
    f (x :/: Con (-1))  = return $ Neg x
    f (Neg x :/: Neg y) = return $ x :/: y
    f (Con x :/: Con y) | x<0 && y<0 = return $ Con (negate x) :/: Con (negate y)
                        | otherwise = Nothing
    f (x :+: Neg y)     = return $ x :-: y
    f (x :+: Con y)     | y<0 = return $ x :-: (Con $ negate y)
                        | otherwise = Nothing
    f (x :-: Neg y)     = return $ x :+: y
    f (x :-: Con y)     | y < 0 = return $ x :+: (Con $ negate y)
--    f (Neg x)           = return $ pushNeg' x -- non-recursive??
    f _                 = Nothing
  
rulePushNeg :: FracRule -- push Negs inside
rulePushNeg = makeSimpleRule "PushNeg" f
  where
   f (Neg x) = case x of
                (Var x)   -> Nothing
                (Con x)   -> return $ Con (negate x)
                (b :*: c) -> return $ Neg b :*: c
                (b :/: c) -> return $ Neg b :/: c
                (b :+: c) -> return $ Neg b :-: c
                (b :-: c) -> return $ Neg b :+: c
                (Neg b)   -> return $ b
   f _         = Nothing

ruleNegToCon :: FracRule
ruleNegToCon = minorRule $ makeSimpleRule "NegToCon" f
  where
    f (Neg (Con x)) = return $ Con (negate x)
    f _             = Nothing

-- | Fraction rules
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
