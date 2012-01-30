-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
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

import Common.Library hiding (root)
import Control.Monad
import Data.Maybe
import Domain.Math.Data.Polynomial
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import Domain.Math.Polynomial.Views
import Domain.Math.Power.Views
import Prelude hiding ((^))

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

lambda :: String -> Expr -> Expr
lambda = binary lambdaSymbol . Var

diffId :: Id
diffId = newId "calculus.differentiation"

isDiffSymbol, isLambdaSymbol :: Symbol -> Bool
isDiffSymbol   = (== diffSymbol)
isLambdaSymbol = (== lambdaSymbol)

-----------------------------------------------------------------
-- Rules for Diffs

ruleSine :: Rule Expr
ruleSine = rewriteRule (diffId, "sine") $
   \x -> diff (lambda x (sin (Var x)))  :~>  cos (Var x)

ruleLog :: Rule Expr
ruleLog = rewriteRule (diffId, "logarithmic") $
   \x -> diff (lambda x (ln (Var x)))  :~>  1 / Var x

ruleDerivPlus :: Rule Expr
ruleDerivPlus = rewriteRule (diffId, "plus") $
   \x f g -> diff (lambda x (f + g))  :~>  diff (lambda x f) + diff (lambda x g)

ruleDerivMin :: Rule Expr
ruleDerivMin = rewriteRule (diffId, "min") $
   \x f g -> diff (lambda x (f - g))  :~>  diff (lambda x f) - diff (lambda x g)

ruleDerivNegate :: Rule Expr
ruleDerivNegate = rewriteRule (diffId, "negate") $
   \x f -> diff (lambda x (-f))  :~>  -diff (lambda x f)

ruleDerivVar :: Rule Expr
ruleDerivVar = rewriteRule (diffId, "var") $
   \x -> diff (lambda x (Var x))  :~>  1

ruleDerivProduct :: Rule Expr
ruleDerivProduct = rewriteRule (diffId, "product") $
   \x f g -> diff (lambda x (f * g))  :~>  diff (lambda x f)*g + f*diff (lambda x g)

-- The second rewrite rule should not have been necessary, except that cleaning
-- up an expression will typically put the negate in front of the division: this
-- makes sure the rule is triggered anyway.
ruleDerivQuotient :: Rule Expr
ruleDerivQuotient = rewriteRules (diffId, "quotient")
   [ \x f g -> diff (lambda x (f/g))  :~>  (g*diff (lambda x f) - f*diff (lambda x g)) / (g^2)
   , \x f g -> diff (lambda x (-f/g))  :~>  (g*diff (lambda x (-f)) - (-f)*diff (lambda x g)) / (g^2)
   ]

ruleDerivPolynomial :: Rule Expr
ruleDerivPolynomial = describe "This rule returns the derivative for all \
   \expressions that can be turned into a polynomial (of rational numbers). \
   \The polynomial does not have to be in standard form." $
   makeRule (diffId, "deriv-of-poly") f
 where
   f (Sym d [Sym l [Var v, expr]]) | isDiffSymbol d && isLambdaSymbol l = do
      let myView = polyViewWith rationalView
      (s, p) <- match myView expr
      guard (s==v)
      return (build myView (s, derivative p))
   f _ = Nothing

-----------------------------------
-- Special rules (not defined with unification)

ruleDerivCon :: Rule Expr
ruleDerivCon = makeRule (diffId, "constant") f
 where
   f (Sym d [Sym l [Var x, e]])
      | isDiffSymbol d && isLambdaSymbol l && withoutVar x e = return 0
   f _ = Nothing

ruleDerivMultiple :: Rule Expr
ruleDerivMultiple = makeRule (diffId, "constant-multiple") f
 where
    f (Sym d [Sym l [Var x, n :*: e]])
       | isDiffSymbol d && isLambdaSymbol l && withoutVar x n =
       return $ n * diff (lambda x e)
    f (Sym d [Sym l [Var x, e :*: n]])
       | isDiffSymbol d && isLambdaSymbol l && withoutVar x n =
       return $ n * diff (lambda x e)
    f _ = Nothing

ruleDerivPower :: Rule Expr
ruleDerivPower = makeRule (diffId, "power") f
 where
   f (Sym d [Sym l [Var x, Sym p [x1, n]]])
      | isDiffSymbol d && isLambdaSymbol l && isPowerSymbol p && Var x==x1 && withoutVar x n =
      return $ n * (Var x ^ (n-1))
   f _ = Nothing

ruleDerivPowerChain :: Rule Expr
ruleDerivPowerChain = makeRule (diffId, "chain-power") f
 where
   f (Sym d [Sym l [Var x, Sym p [a, n]]])
      | isDiffSymbol d && isLambdaSymbol l && isPowerSymbol p && withoutVar x n =
      return $ n * (a ^ (n-1)) * diff (lambda x a)
   f _ = Nothing

ruleDerivSqrt :: Rule Expr
ruleDerivSqrt = makeRule (diffId, "sqrt") f
 where
   f (Sym d [Sym l [Var x, Sqrt x1]])
      | isDiffSymbol d && isLambdaSymbol l && Var x==x1 =
      return $ 1 / (2 * sqrt (Var x))
   f _ = Nothing

ruleDerivSqrtChain :: Rule Expr
ruleDerivSqrtChain = makeRule (diffId, "chain-sqrt") f
 where
   f (Sym d [Sym l [Var x, Sqrt a]])
      | isDiffSymbol d && isLambdaSymbol l =
      return $ (1 / (2 * sqrt a)) * diff (lambda x a)
   f _ = Nothing

ruleDefRoot :: Rule Expr
ruleDefRoot = rewriteRule (diffId, "def-root") $
   \a b -> root a b :~> a ^ (1/b)

ruleDerivRoot :: Rule Expr
ruleDerivRoot = rewriteRule (diffId, "def-root") $
   \a b x -> diff (lambda x (root a b)) :~> diff (lambda x (a ^ (1/b)))

ruleDerivPowerFactor :: Rule Expr
ruleDerivPowerFactor = makeRule (diffId, "power-factor") $ \de -> do
   expr <- getDiffExpr de
   (a, x, r) <- match myPowerView expr
   return $ build myPowerView (a*fromRational r, x, r-1)

-- (a+b)/c  ~>  a/c + b/c
ruleSplitRational :: Rule Expr
ruleSplitRational = makeRule (diffId, "split-rational") $ \expr -> do
   (upper, c) <- match divView expr
   (a, b)     <- match plusView upper
   return (a/c + b/c)

myPowerView :: View Expr (Expr, String, Rational)
myPowerView = makeView f g
 where
   f expr = case match timesView expr of
               Just (a, b) -> do
                  guard (hasNoVar a)
                  (x, r) <- match powView b
                  return (a, x, r)
                `mplus` do
                  guard (hasNoVar b)
                  (x, r) <- match powView a
                  return (b, x, r)
               Nothing -> do
                  (x, r) <- match powView expr
                  return (1, x, r)
   g (a, x, r) = a .*. (Var x .^. fromRational r)

   powView = (matcher powerView <+> matcher noPowerView)
             >>> matcher (variableView *** rationalView)
   noPowerView = makeView (\expr -> Just (expr, 1)) (build powerView)

isDiff :: Expr -> Bool
isDiff = isJust . getDiffExpr

getDiffExpr :: Expr -> Maybe Expr
getDiffExpr (Sym d [Sym l [Var _, expr]]) |
   isDiffSymbol d && isLambdaSymbol l = Just expr
getDiffExpr _ = Nothing