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
    commonPower, nthRoot, sameBase, equalsOne, greatestPower
  , approxPower, reciprocalFor
  ) where

import Common.Transformation
import Common.Rewriting
import Common.View hiding (simplify)
import Control.Monad
import Domain.Math.Approximation (precision)
import qualified Domain.Math.Data.PrimeFactors as PF
import Domain.Math.Data.Relation
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import Domain.Math.Power.Utils
import Domain.Math.Power.Views
import Domain.Math.Simplification (simplify)


-- | Identifier prefix --------------------------------------------------------

powereq :: String
powereq = "algebra.manipulation.exponents.equation"

-- | Power relation rules -----------------------------------------------------

-- | a^x = b^y  =>  a^(x/c) = b^(y/c)  where c = gcd x y
commonPower :: Rule (Equation Expr)
commonPower = makeSimpleRule (powereq, "common-power") $ \expr -> do
  let v = eqView (powerView >>> second integerView)
  ((a, x), (b, y)) <- match v expr
  let c = gcd x y
  guard $ c > 1
  return $ build v ((a, x `div` c), (b, y `div` c))

-- | a^x = n  =>  a^x = b^e
greatestPower :: Rule (Equation Expr)
greatestPower = makeSimpleRule (powereq, "greatest-power") $ \(lhs :==: rhs) -> do
  n      <- match integerView rhs
  (_, x) <- match (powerView >>> second integerView) lhs
  (b, e) <- PF.greatestPower n
  guard $ gcd x e > 1
  return $ lhs :==: fromInteger b .^. fromInteger e

-- a^x = c*b^y  =>  a = c*b^(y/x)
nthRoot :: Rule (Equation Expr)
nthRoot = makeSimpleRule (powereq, "nth-root") $ \(lhs :==: rhs) -> do
  guard $ hasSomeVar lhs
  (a, x)      <- match powerView lhs
  (c, (b, y)) <- match unitPowerView rhs
  return $ a :==: build unitPowerView (c, (b, simplify (y ./. x)))

-- -- root a x = b  =>  a = b^x
-- nthPower :: Rule (Equation Expr)
-- nthPower = makeSimpleRule (powereq, "nth-power") $ \(lhs :==: rhs) -> do
--   guard $ hasSomeVar lhs
--   (a, x) <- match rootView lhs
--   return $ a :==: rhs .^. x

-- x = a^x  =>  x ~= d
approxPower :: Rule (Relation Expr)
approxPower = makeRule (powereq, "approx-power") $ approxPowerT 2

-- x = a^x  =>  x ~= d
approxPowerT :: Int -> Transformation (Relation Expr)
approxPowerT n = makeTrans $ \ expr ->
  match equationView expr >>= f
  where
    f (Var x :==: d) = 
      match doubleView d >>= Just . (Var x .~=.) . Number . precision n
    f (d :==: Var x) = 
      match doubleView d >>= Just . (.~=. Var x) . Number . precision n
    f _              = Nothing

-- -- a*x + c = b*y + d  =>  a*x - b*y = d - c   (move vars to the left, cons to the right)
-- varLeftConRight :: Rule (Equation Expr)
-- varLeftConRight = makeSimpleRule (powereq, "var-left-con-right") $ 
--   \(lhs :==: rhs) -> do
--     (xs, cs) <- fmap (partition hasSomeVar) (match sumView lhs)
--     (ys, ds) <- fmap (partition hasSomeVar) (match sumView rhs)
--     guard $ length cs > 0 || length ys > 0
--     return $ fmap collectLikeTerms $ 
--       build sumView (xs ++ map neg ys) :==: build sumView (ds ++ map neg cs)

-- a^x = a^y  =>  x = y
sameBase :: Rule (Equation Expr)
sameBase = makeSimpleRule (powereq, "same-base") $ \ expr -> do
  ((a, x), (b, y)) <- match (eqView powerView) expr
  guard $ a == b
  return $ x :==: y

-- | c*a^x = d*(1/a)^y  => c*a^x = d*a^-y
-- this reciprocal rule is more strict, it demands a same base on the lhs
-- of the equation. Perhaps do this via the enviroment?
reciprocalFor :: Rule (Equation Expr)
reciprocalFor = makeSimpleRule (powereq, "reciprocal-for-base") $ 
  \ (lhs :==: rhs) -> do
    (_, (a,  _)) <- match unitPowerView lhs
    (one, _)     <- match divView rhs
    (d, (a'', y)) <- match consPowerView rhs
    guard $ one == 1 && a'' == a
    return $ lhs :==: d .*. a'' .^. negate y

-- | a^x = 1  =>  x = 0
equalsOne :: Rule (Equation Expr)
equalsOne = makeSimpleRule (powereq, "equals-one") $ \ (lhs :==: rhs) -> do
  guard $ rhs == 1
  (_, x) <- match powerView lhs
  return $ x :==: 0

