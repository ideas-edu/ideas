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
fracRules = [ ruleAddZero, ruleSubZero
             ]

-- local frac variables
x, y, z :: Frac
x:y:z:_ = map makeVarInt [0..]

ruleAddZero :: FracRule
ruleAddZero = makeRuleList "AddZero"
   [ (x :+: Lit 0)  |-  x
   , (Lit 0 :+: x)  |-  x
   ]
   
ruleSubZero :: FracRule
ruleSubZero = makeRuleList "SubZero"
   [ (x :-: Lit 0)  |-  x
   , (Lit 0 :+: x)  |-  x
   ]

ruleMulZero :: FracRule
ruleMulZero = makeRule "MulZero"
   [ (x :*: Lit 0)  |-  Lit 0
   , (Lit 0 :*: x)  |-  Lit 0
   ]

ruleMulOne :: FracRule
ruleMulOne = makeRule "MulOne"
   [ (x :*: Lit 1)  |-  x
   , (Lit 1 :*: x)  |-  x
   ]

ruleDivOne :: FracRule
ruleDivOne = makeRule "DivOne"
   [ (x :/: Lit 1)  |-  x
   , (Lit 1 :/: x)  |-  x
   ]

ruleDivReciprocal :: FracRule
ruleDivReciprocal = makeRule "DivReciprocal" $
   (x :/: (y :/: z)) |- ((x :*: z) :/: y)

ruleAdd :: FracRule
ruleAdd = makeRule "Add" $
   (Lit x :+: Lit y)  |-  Lit (x+y)
 
ruleSub :: FracRule
ruleSub = makeRule "Sub" $
   (Lit x :-: Lit y)  |-  Lit (x-y)

ruleDiv :: FracRule
ruleDiv = makeRule "Div" $
   (Lit x :/: Lit y)  |-  Lit (x/y) --check y non zero

ruleMul :: FracRule
ruleMul = makeRule "Mul" $
   (Lit x :*: Lit y)  |-  Lit (x*y)
  
-- todo:  distribution, negate, 
