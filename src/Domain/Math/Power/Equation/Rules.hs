-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Domain.Math.Power.Equation.Rules 
  ( -- * Power equation rules
  , factorAsPower, commonPower, nthRoot, sameBase, equalsOne, greatestPower
  , nthPower, approxPower, varLeftConRight, reciprocalFor
  ) where

import Common.Classes
import Control.Arrow ( (>>^) )
import Common.Id
import Common.Transformation
import Common.View
import Control.Monad
import Data.List
import Data.Maybe
import Data.Ratio
import Domain.Math.Approximation (precision)
import qualified Domain.Math.Data.PrimeFactors as PF
import Domain.Math.Data.Relation
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import Domain.Math.Power.Views
import Domain.Math.Polynomial.CleanUp

powereq = "algebra.manipulation.exponents.equation"


-- | Power relation rules -----------------------------------------------------

-- a^x = b^y  =>  a^(x/c) = b^(y/c)  where c = gcd x y
commonPower :: Rule (Equation Expr)
commonPower = makeSimpleRule (power, "common-power") $ \(lhs :==: rhs) -> do
    (a, x) <- match simplePowerView lhs
    x'     <- match integerView x
    (b, y) <- match simplePowerView rhs
    y'     <- match integerView y
    let c = gcd x' y'
    guard (c > 1)
    return $ a .^. build integerView (x' `div` c) :==: 
             b .^. build integerView (y' `div` c)

greatestPower :: Rule (Equation Expr)
greatestPower = makeSimpleRule (power, "greatest-power") $ \(lhs :==: rhs) -> do
  n      <- match natView rhs
  _ <- match simplePowerView lhs >>= match integerView. snd
  (a, x) <- PF.greatestPower $ toInteger n
  return $ lhs :==: fromInteger a .^. fromInteger x
    
-- a^x = b^y  =>  a = b^(y/x) = root x b^y  where y may be one
nthRoot :: Rule (Equation Expr)
nthRoot = makeSimpleRule (power, "nth-root") $ \(lhs :==: rhs) -> do
  guard $ hasVars lhs
  (a, x) <- match simplePowerView lhs
  (b, y) <- match (simplePowerView <&> (identity >>^ \x -> (x,1))) rhs
  return $ a :==: b .^. (y ./. x)

-- root a x = b  =>  a = b^x
nthPower :: Rule (Equation Expr)
nthPower = makeSimpleRule (power, "nth-power") $ \(lhs :==: rhs) -> do
  guard $ hasVars lhs
  (a, x) <- match simpleRootView lhs
  return $ a :==: rhs .^. x

-- x = a^x  =>  x ~= d
approxPower :: Rule (Relation Expr)
approxPower = makeSimpleRule (power, "approx-power") $ \ expr ->
  match equationView expr >>= f
  where
    f (Var x :==: d) = match doubleView d >>= Just . (Var x .~=.) . Number . precision 2 
    f (d :==: Var x) = match doubleView d >>= Just . (.~=. Var x) . Number . precision 2
    f _              = Nothing

-- a*x + c = b*y + d  =>  a*x - b*y = d - c   (move vars to the left, cons to the right)
varLeftConRight :: Rule (Equation Expr)
varLeftConRight = makeSimpleRule (power, "var-left-con-right") $ 
  \(lhs :==: rhs) -> do
    (xs, cs) <- match sumView lhs >>= return . partition hasVars
    (ys, ds) <- match sumView rhs >>= return . partition hasVars
    guard $ length cs > 0 || length ys > 0
    return $ fmap collectLikeTerms $ 
      build sumView (xs ++ map neg ys) :==: build sumView (ds ++ map neg cs)

-- a^x = a^y  =>  x = y
sameBase :: Rule (Equation Expr)
sameBase = makeSimpleRule (power, "same-base") $ \(lhs :==: rhs) -> do
    (a, x) <- match simplePowerView lhs
    (b, y) <- match simplePowerView rhs
    guard (a == b)
    return $ x :==: y

-- | c*a^x = d*(1/a)^y  => c*a^x = d*a^-y
-- this reciprocal rule is more strict, it demands a same base on the lhs
-- of the equation. Perhaps do this via the enviroment?
reciprocalFor :: Rule (Equation Expr)
reciprocalFor = makeSimpleRule (power, "reciprocal-for-base") $ \ (lhs :==: rhs) -> do
  (_, (a,  _)) <- match strictPowerView lhs
  (d, (a', y)) <- match strictPowerView rhs
  (one, a'')   <- match divView a'
  guard $ one == 1 && a'' == a
  return $ lhs :==: d .*. a'' .^. (negate y)

-- | a^x = 1  =>  x = 0
equalsOne :: Rule (Equation Expr)
equalsOne = makeSimpleRule (power, "equals-one") $ \ (lhs :==: rhs) -> do
  guard $ rhs == 1
  (_, x) <- match simplePowerView lhs
  return $ x :==: 0

