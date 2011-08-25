-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
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
  -- ( -- * Power equation rules
  --   commonPower, nthRoot, sameBase, equalsOne, greatestPower
  -- , approxPower, reciprocalFor, coverUpRootWith, coverUpRoot
  -- )
  where

import Common.Library hiding (simplify)
import Control.Monad
--import Data.List (partition)
import Domain.Math.Approximation (precision)
import Domain.Math.Data.Relation
import Domain.Math.Equation.CoverUpRules
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import qualified Domain.Math.Data.PrimeFactors as PF
--import Domain.Math.CleanUp (collectLikeTerms)
import Domain.Math.Polynomial.Rules (distributeTimes, distributeDivisionT)
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

-- x = a^x  =>  x ~= d
approxPower :: Rule (Relation Expr)
approxPower = makeRule (powereq, "approx-power") $ approxPowerT 2

-- x = a^x  =>  x ~= d
approxPowerT :: Int -> Transformation (Relation Expr)
approxPowerT n = makeTrans $ \ expr ->
  match equationView expr >>= f
  where
    f (Var x :==: d) =
      match doubleView d >>= Just . (Var x .~=.) . fromDouble . precision n
    f (d :==: Var x) =
      match doubleView d >>= Just . (.~=. Var x) . fromDouble . precision n
    f _              = Nothing

-- a^x = a^y  =>  x = y
sameBase :: Rule (Equation Expr)
sameBase = makeSimpleRule (powereq, "same-base") $ \ expr -> do
  ((a, x), (b, y)) <- match (eqView powerView) expr
  guard $ a == b
  return $ x :==: y

-- | c*a^x = d*(1/a)^y  => c*a^x = d*a^-y
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

----------------------- Move these funcs to right place ----------------------

-- add these two functions to coverUpRules?
coverUpRootWith :: ConfigCoverUp -> Rule (Equation Expr)
coverUpRootWith = coverUpBinaryRule "root" (isBinary rootSymbol) (.^.)

coverUpRoot :: Rule (Equation Expr)
coverUpRoot = coverUpRootWith configCoverUp

-- | Negations are pushed inside
myCoverUpTimesWith :: ConfigCoverUp -> Rule (Equation Expr)
myCoverUpTimesWith = doAfter f . coverUpTimesWith
  where f = mapRight (applyD distributeDivisionT . applyD distributeTimes)

condXisRight :: Rule (Equation Expr)
condXisRight = describe "flip condition" $ checkRule $ \(lhs :==: rhs) ->
   hasVar "x" rhs && withoutVar "x" lhs

--xToLeft = makeRule (powereq, "x -to-left") $  toLeftRightT $ elem "x" . vars

-- toLeftRightT :: (Expr -> Bool) -> Transformation (Equation Expr)
-- toLeftRightT p = makeTrans $
--   \ (lhs :==: rhs) -> do
--     (xs, cs) <- fmap (partition p) (match sumView lhs)
--     (ys, ds) <- fmap (partition p) (match sumView rhs)
--     guard $ length cs > 0 || length ys > 0
--     return $ fmap collectLikeTerms $
--       build sumView (xs ++ map neg ys) :==: build sumView (ds ++ map neg cs)