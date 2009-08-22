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
module Domain.Math.DerivativeRules where

import Prelude hiding ((^))
import Common.Transformation
import Domain.Math.Expr
import Domain.Math.Expr.Symbolic
import Domain.Math.Expr.Symbols
import Common.Rewriting

derivativeRules :: [Rule Expr]
derivativeRules =
   [ ruleDerivCon, ruleDerivPlus, ruleDerivMin
   , ruleDerivMultiple, ruleDerivPower, ruleDerivVar 
   , ruleDerivProduct, ruleDerivQuotient {-, ruleDerivChain-}, ruleDerivChainPowerExprs
   , ruleSine, ruleLog 
   ]

diff :: Expr -> Expr
diff = unary diffSymbol

ln :: Expr -> Expr
ln = unary lnSymbol

lambda :: Expr -> Expr -> Expr
lambda = binary lambdaSymbol

fcomp :: Expr -> Expr -> Expr
fcomp = binary fcompSymbol

-----------------------------------------------------------------
-- Rules for Diffs

ruleSine :: Rule Expr
ruleSine = rule "Sine" $ 
   \x -> diff (lambda x (sin x))  :~>  lambda x (cos x)

ruleLog :: Rule Expr
ruleLog = rule "Logarithmic" $
   \x -> diff (lambda x (ln x))  :~>  lambda x (1/x)
       
ruleDerivPlus :: Rule Expr
ruleDerivPlus = rule "Sum" $
   \x f g -> diff (lambda x (f + g))  :~>  diff (lambda x f) + diff (lambda x g)

ruleDerivMin :: Rule Expr
ruleDerivMin = rule "Sum" $
   \x f g -> diff (lambda x (f - g))  :~>  diff (lambda x f) - diff (lambda x g)

ruleDerivVar :: Rule Expr
ruleDerivVar = rule "Var" $
   \x -> diff (lambda x x)  :~>  1

ruleDerivProduct :: Rule Expr
ruleDerivProduct = rule "Product" $
   \x f g -> diff (lambda x (f * g))  :~>  f*diff (lambda x g) + g*diff (lambda x f)
       
ruleDerivQuotient :: Rule Expr
ruleDerivQuotient = rule "Quotient" $ 
   \x f g -> diff (lambda x (f/g))  :~>  (g*diff (lambda x f) - f*diff (lambda x g)) / (g^2)

{- ruleDerivChain :: Rule Expr
ruleDerivChain = rule "Chain Rule" f
 where f (Diff x (f :.: g)) = return $ (Diff x f :.: g) :*: Diff x g
       f _                        = Nothing -}

-----------------------------------
-- Special rules (not defined with unification)

ruleDerivCon :: Rule Expr
ruleDerivCon = makeSimpleRule "Constant Term" f
 where 
   f (Sym d [Sym l [Var v, e]]) 
      | d == diffSymbol && l == lambdaSymbol && v `notElem` collectVars e = return 0
   f _ = Nothing
 
ruleDerivMultiple :: Rule Expr
ruleDerivMultiple = makeSimpleRule "Constant Multiple" f
 where 
    f (Sym d [Sym l [x@(Var v), n :*: e]]) 
       | d == diffSymbol && l == lambdaSymbol && v `notElem` collectVars n = 
       return $ n * diff (lambda x e)
    f (Sym d [Sym l [x@(Var v), e :*: n]]) 
       | d == diffSymbol && l == lambdaSymbol && v `notElem` collectVars n = 
       return $ n * diff (lambda x e)
    f _ = Nothing 

ruleDerivPower :: Rule Expr
ruleDerivPower = makeSimpleRule "Power" f
 where 
   f (Sym d [Sym l [x@(Var v), Sym p [x1, n]]]) 
      | d == diffSymbol && l == lambdaSymbol && p == powerSymbol && x==x1 && v `notElem` collectVars n =
      return $ n * (x ^ (n-1)) 
   f _ = Nothing

ruleDerivChainPowerExprs :: Rule Expr
ruleDerivChainPowerExprs = makeSimpleRule "Chain Rule for Power Exprs" f 
 where 
   f (Sym d [Sym l [x@(Var v), Sym p [g, n]]]) 
      | d == diffSymbol && l == lambdaSymbol && p == powerSymbol && v `notElem` collectVars n =
      return $ n * (g ^ (n-1)) * diff (lambda x g)
   f _ = Nothing