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
-----------------------------------------------------------------------------
module Domain.Derivative.Rules where

import Domain.Derivative.Basic
import Common.Context
import Common.Transformation

derivativeRules :: [Rule (Context Expr)]
derivativeRules = map liftRuleToContext
   [ ruleDerivCon, ruleDerivPlus, ruleDerivMultiple, ruleDerivPower, ruleDerivVar 
   , ruleDerivProduct, ruleDerivQuotient {-, ruleDerivChain-}, ruleDerivChainPowerExprs
   , ruleSine, ruleLog 
   ]

tidyupRules :: [Rule (Context Expr)]
tidyupRules = [liftRuleToContext tidyRule]

-----------------------------------------------------------------
-- Rules for Diffs

ruleSine :: Rule Expr
ruleSine = makeSimpleRule "Sine Rule" f
 where f (Diff (Lambda _ (Special Sin x))) = return $ Special Cos x
       f _ = Nothing

ruleLog :: Rule Expr
ruleLog = makeSimpleRule "Logarithmic Rule" f
 where f (Diff (Lambda _ (Special Ln x))) = return $ Con 1 :/: x
       f _ = Nothing

ruleDerivCon :: Rule Expr
ruleDerivCon = makeSimpleRule "Constant Term Rule" f
 where f (Diff (Lambda _ (Con _))) = return $ Con 0
       f _                         = Nothing
       
ruleDerivPlus :: Rule Expr
ruleDerivPlus = makeSimpleRule "Sum Rule" f 
 where f (Diff (Lambda x (f :+: g))) = return $ Diff (Lambda x f) :+: Diff (Lambda x g)
       f _                           = Nothing
       
ruleDerivMultiple :: Rule Expr
ruleDerivMultiple = makeSimpleRule "Constant Multiple Rule" f
 where f (Diff (Lambda x (Con n :*: f))) = return $ Con n :*: Diff (Lambda x f)
       f _                               = Nothing
       
ruleDerivPower :: Rule Expr
ruleDerivPower = makeSimpleRule "Power Rule" f
 where f (Diff (Lambda x (Var x1 :^: Con n))) | x==x1 = return $ Con n :*: (Var x :^: Con (n-1)) 
       f _ = Nothing

ruleDerivVar :: Rule Expr
ruleDerivVar = makeSimpleRule "Var Rule" f
 where f (Diff (Lambda x (Var x1))) | x==x1 = return $ Con 1
       f _ = Nothing

ruleDerivProduct :: Rule Expr
ruleDerivProduct = makeSimpleRule "Product Rule" f
 where f (Diff (Lambda x (f :*: g))) = return $ (f :*: Diff (Lambda x g)) :+: (g :*: Diff (Lambda x f))
       f _                           = Nothing
       
ruleDerivQuotient :: Rule Expr
ruleDerivQuotient = makeSimpleRule "Quotient Rule" f
 where f (Diff (Lambda x (f :/: g))) = return $ ((g :*: Diff (Lambda x f)) :+: (Con (-1) :*: f :*: Diff (Lambda x g))) :/: (g :^: Con 2) 
       f _                        = Nothing
       
{- ruleDerivChain :: Rule Expr
ruleDerivChain = makeSimpleRule "Chain Rule" f
 where f (Diff x (f :.: g)) = return $ (Diff x f :.: g) :*: Diff x g
       f _                        = Nothing -}
 
ruleDerivChainPowerExprs :: Rule Expr
ruleDerivChainPowerExprs = makeSimpleRule "Chain Rule for Power Exprs" f 
 where f (Diff (Lambda x (f :^: Con n))) = return $ Con n :*: (f :^: Con (n-1)) :*: Diff (Lambda x f)
       f _                               = Nothing

tidyRule :: Rule Expr
tidyRule = makeSimpleRule "Tidy-up rule" f
 where
   f (Con a :+: Con b) = return $ Con (a+b)
   f (Con a :*: Con b) = return $ Con (a*b)
   -- f (Con a :^: Con b) = return $ Con (a**b)
   f (Con a :/: Con b) = return $ Con (a/b)
   f (Con a :-: Con b) = return $ Con (a-b)
   f (Negate (Con a))  = return $ Con (negate a)
   
   f (x :*: (y :*: z)) = return $ (x :*: y) :*: z
   
   f (Con 1 :*: x) = return x
   f (x :*: Con 1) = return x
   f (Con 0 :+: x) = return x
   f (x :+: Con 0) = return x
   f (x :^: Con 0) = return $ Con 1
   f (x :^: Con 1) = return x

   f _ = Nothing