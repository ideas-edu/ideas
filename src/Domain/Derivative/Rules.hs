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

type Fun = Function

derivativeRules :: [Rule (Context Function)]
derivativeRules = map liftRuleToContext
   [ ruleDerivCon, ruleDerivPlus, ruleDerivMultiple, ruleDerivPower, ruleDerivVar 
   , ruleDerivProduct, ruleDerivQuotient, ruleDerivChain, ruleDerivChainPowerFunctions
   ]

tidyupRules :: [Rule (Context Function)]
tidyupRules = [liftRuleToContext tidyRule]

-----------------------------------------------------------------
-- Rules for derivatives

ruleDerivCon :: Rule Fun
ruleDerivCon = makeSimpleRule "Constant Term Rule" f
 where f (Derivative _ (Con _)) = return $ Con 0
       f _                      = Nothing
       
ruleDerivPlus :: Rule Fun
ruleDerivPlus = makeSimpleRule "Sum Rule" f 
 where f (Derivative x (f :+: g)) = return $ Derivative x f :+: Derivative x g
       f _                        = Nothing
       
ruleDerivMultiple :: Rule Fun
ruleDerivMultiple = makeSimpleRule "Constant Multiple Rule" f
 where f (Derivative x (Con n :*: f)) = return $ Con n :*: Derivative x f
       f _                            = Nothing
       
ruleDerivPower :: Rule Fun
ruleDerivPower = makeSimpleRule "Power Rule" f
 where f (Derivative x (Var x1 :^: Con n)) | x==x1 = return $ Con n :*: (Var x :^: Con (n-1)) 
       f _ = Nothing

ruleDerivVar :: Rule Fun
ruleDerivVar = makeSimpleRule "Var Rule" f
 where f (Derivative x (Var x1)) | x==x1 = return $ Con 1
       f _ = Nothing

ruleDerivProduct :: Rule Fun
ruleDerivProduct = makeSimpleRule "Product Rule" f
 where f (Derivative x (f :*: g)) = return $ (f :*: Derivative x g) :+: (g :*: Derivative x f)
       f _                        = Nothing
       
ruleDerivQuotient :: Rule Fun
ruleDerivQuotient = makeSimpleRule "Quotient Rule" f
 where f (Derivative x (f :/: g)) = return $ ((g :*: Derivative x f) :+: (Con (-1) :*: f :*: Derivative x g)) :/: (g :^: Con 2) 
       f _                        = Nothing
       
ruleDerivChain :: Rule Fun
ruleDerivChain = makeSimpleRule "Chain Rule" f
 where f (Derivative x (f :.: g)) = return $ (Derivative x f :.: g) :*: Derivative x g
       f _                        = Nothing
 
ruleDerivChainPowerFunctions :: Rule Fun
ruleDerivChainPowerFunctions = makeSimpleRule "Chain Rule for Power Functions" f 
 where f (Derivative x (f :^: Con n)) = return $ Con n :*: (f :^: Con (n-1)) :*: Derivative x f
       f _                            = Nothing

tidyRule :: Rule Fun
tidyRule = makeSimpleRule "Tidy-up rule" f
 where
   f (Con a :+: Con b) = return $ Con (a+b)
   f (Con a :*: Con b) = return $ Con (a*b)
   -- f (Con a :^: Con b) = return $ Con (a**b)
   f (Con a :/: Con b) = return $ Con (a/b)
   
   f (x :*: (y :*: z)) = return $ (x :*: y) :*: z
   
   f (Con 1 :*: x) = return x
   f (x :*: Con 1) = return x
   f (Con 0 :+: x) = return x
   f (x :+: Con 0) = return x
   f (x :^: Con 0) = return $ Con 1
   f (x :^: Con 1) = return x
   
   f _ = Nothing