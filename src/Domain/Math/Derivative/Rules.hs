-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Domain.Math.Derivative.Rules where

import Prelude hiding ((^))
import Common.Transformation
import Common.View
import Control.Monad
import Domain.Math.Expr
import Common.Id
import Common.Rewriting
import Data.Maybe
import Domain.Math.Polynomial.Views
import Domain.Math.Numeric.Views
import Domain.Math.Data.Polynomial
import Domain.Math.Power.Views

derivativeRules :: [Rule Expr]
derivativeRules =
   [ ruleDerivCon, ruleDerivPlus, ruleDerivMin, ruleDerivNegate
   , ruleDerivMultiple, ruleDerivPower, ruleDerivVar 
   , ruleDerivProduct, ruleDerivQuotient, ruleDerivPowerChain
   , ruleSine, ruleLog, ruleDerivSqrt, ruleDerivSqrtChain
   ]

diff :: Expr -> Expr
diff = unary diffSymbol

ln :: Expr -> Expr
ln = unary lnSymbol

lambda :: Expr -> Expr -> Expr
lambda = binary lambdaSymbol

diffId :: Id
diffId = newId "calculus.differentiation"

-----------------------------------------------------------------
-- Rules for Diffs

ruleSine :: Rule Expr
ruleSine = rule (diffId, "sine") $ 
   \x -> diff (lambda x (sin x))  :~>  cos x

ruleLog :: Rule Expr
ruleLog = rule (diffId, "logarithmic") $
   \x -> diff (lambda x (ln x))  :~>  1/x
       
ruleDerivPlus :: Rule Expr
ruleDerivPlus = rule (diffId, "plus") $
   \x f g -> diff (lambda x (f + g))  :~>  diff (lambda x f) + diff (lambda x g)

ruleDerivMin :: Rule Expr
ruleDerivMin = rule (diffId, "min") $
   \x f g -> diff (lambda x (f - g))  :~>  diff (lambda x f) - diff (lambda x g)

ruleDerivNegate :: Rule Expr
ruleDerivNegate = rule (diffId, "negate") $
   \x f -> diff (lambda x (-f))  :~>  -diff (lambda x f)

ruleDerivVar :: Rule Expr
ruleDerivVar = rule (diffId, "var") $
   \x -> diff (lambda x x)  :~>  1

ruleDerivProduct :: Rule Expr
ruleDerivProduct = rule (diffId, "product") $
   \x f g -> diff (lambda x (f * g))  :~>  diff (lambda x f)*g + f*diff (lambda x g)

-- The second rewrite rule should not have been necessary, except that cleaning
-- up an expression will typically put the negate in front of the division: this
-- makes sure the rule is triggered anyway.
ruleDerivQuotient :: Rule Expr
ruleDerivQuotient = ruleList (diffId, "quotient") 
   [ \x f g -> diff (lambda x (f/g))  :~>  (g*diff (lambda x f) - f*diff (lambda x g)) / (g^2)
   , \x f g -> diff (lambda x (-f/g))  :~>  (g*diff (lambda x (-f)) - (-f)*diff (lambda x g)) / (g^2)
   ]

ruleDerivPolynomial :: Rule Expr
ruleDerivPolynomial = describe "This rule returns the derivative for all \
   \expressions that can be turned into a polynomial (of rational numbers). \
   \The polynomial does not have to be in standard form." $ 
   makeSimpleRule (diffId, "deriv-of-poly") f
 where
   f (Sym d [Sym l [Var v, expr]]) | d == diffSymbol && l == lambdaSymbol = do
      let myView = polyViewWith rationalView
      (s, p) <- match myView expr
      guard (s==v)
      return (build myView (s, derivative p))
   f _ = Nothing
   
-----------------------------------
-- Special rules (not defined with unification)

ruleDerivCon :: Rule Expr
ruleDerivCon = makeSimpleRule (diffId, "constant") f
 where 
   f (Sym d [Sym l [Var v, e]]) 
      | d == diffSymbol && l == lambdaSymbol && v `notElem` collectVars e = return 0
   f _ = Nothing
 
ruleDerivMultiple :: Rule Expr
ruleDerivMultiple = makeSimpleRule (diffId, "constant-multiple") f
 where 
    f (Sym d [Sym l [x@(Var v), n :*: e]]) 
       | d == diffSymbol && l == lambdaSymbol && v `notElem` collectVars n = 
       return $ n * diff (lambda x e)
    f (Sym d [Sym l [x@(Var v), e :*: n]]) 
       | d == diffSymbol && l == lambdaSymbol && v `notElem` collectVars n = 
       return $ n * diff (lambda x e)
    f _ = Nothing 

ruleDerivPower :: Rule Expr
ruleDerivPower = makeSimpleRule (diffId, "power") f
 where 
   f (Sym d [Sym l [x@(Var v), Sym p [x1, n]]]) 
      | d == diffSymbol && l == lambdaSymbol && p == powerSymbol && x==x1 && v `notElem` collectVars n =
      return $ n * (x ^ (n-1)) 
   f _ = Nothing

ruleDerivPowerChain :: Rule Expr
ruleDerivPowerChain = makeSimpleRule (diffId, "chain-power") f 
 where 
   f (Sym d [Sym l [x@(Var v), Sym p [a, n]]]) 
      | d == diffSymbol && l == lambdaSymbol && p == powerSymbol && v `notElem` collectVars n =
      return $ n * (a ^ (n-1)) * diff (lambda x a)
   f _ = Nothing
   
ruleDerivSqrt :: Rule Expr
ruleDerivSqrt = makeSimpleRule (diffId, "sqrt") f
 where
   f (Sym d [Sym l [x@(Var _), Sqrt x1]]) 
      | d == diffSymbol && l == lambdaSymbol && x==x1 =
      return $ 1 / (2 * sqrt x) 
   f _ = Nothing 
   
ruleDerivSqrtChain :: Rule Expr
ruleDerivSqrtChain = makeSimpleRule (diffId, "chain-sqrt") f
 where
   f (Sym d [Sym l [x@(Var _), Sqrt a]]) 
      | d == diffSymbol && l == lambdaSymbol =
      return $ (1 / (2 * sqrt a)) * diff (lambda x a)
   f _ = Nothing 
   
ruleDefRoot :: Rule Expr
ruleDefRoot = rule (diffId, "def-root") $
   \a b -> root a b :~> a ^ (1/b)
   
ruleDerivPowerFactor :: Rule Expr
ruleDerivPowerFactor = makeSimpleRule (diffId, "power-factor") $ \de -> do
   expr <- getDiffExpr de
   (a, x, r) <- match myPowerView expr
   return $ build myPowerView (a*fromRational r, x, r-1)
   
-- (a+b)/c  ~>  a/c + b/c
ruleSplitRational :: Rule Expr
ruleSplitRational = makeSimpleRule (diffId, "split-rational") $ \expr -> do
   (up, c) <- match divView expr
   (a, b)  <- match plusView up
   return (a/c + b/c)
   
myPowerView :: View Expr (Expr, String, Rational)
myPowerView = makeView f g
 where
   f expr = case match timesView expr of
               Just (a, b) -> do
                  guard (noVars a)
                  (x, r) <- match powView b
                  return (a, x, r)
                `mplus` do
                  guard (noVars b)
                  (x, r) <- match powView a
                  return (b, x, r)
               Nothing -> do
                  (x, r) <- match powView expr
                  return (1, x, r)
   g (a, x, r) = a .*. (Var x .^. fromRational r) 
  
   powView = simplePowerView >>> varView *** rationalView
   varView = makeView isVar Var
   
   isVar (Var x) = Just x
   isVar _       = Nothing
   
isDiff :: Expr -> Bool
isDiff = isJust . getDiffExpr

getDiffExpr :: Expr -> Maybe Expr
getDiffExpr (Sym d [Sym l [Var _, expr]]) | 
   d == diffSymbol && l == lambdaSymbol = Just expr
getDiffExpr _ = Nothing