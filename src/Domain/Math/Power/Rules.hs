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

module Domain.Math.Power.Rules 
  ( -- * Power rules
    calcPower, calcPowerPlus, calcPowerMinus, addExponents, mulExponents
  , subExponents, distributePower, distributePowerDiv, reciprocal
  , reciprocalInv, reciprocalFrac, calcPowerRatio, calcRoot, simplifyPower
  , onePower, powerOne, zeroPower, powerZero, divBase, reciprocalVar
  , reciprocalPower, factorAsPower, calcRootHead, simpleAddExponents
    -- * Root rules
  , power2root, root2power
    -- * Log rules
  , logarithm
    -- * Common rules
  , myFractionTimes, pushNegOut
  ) where

import Prelude hiding ( (^) )
import qualified Prelude

import Common.Classes
import Control.Arrow ( (>>^) )
import Common.Id
import Common.Transformation
import Common.Utils (safeHead)
import Common.View
import Control.Monad
import Data.List
import Data.Maybe
import qualified Domain.Math.Data.PrimeFactors as PF
import Domain.Math.Data.OrList
import Domain.Math.Data.Relation
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import Domain.Math.Power.Utils
import Domain.Math.Power.Views


-- | Identifier prefixes ------------------------------------------------------

power, logarithmic :: String
power       = "algebra.manipulation.exponents"
logarithmic = "algebra.manipulation.logarithmic"


-- | Power rules --------------------------------------------------------------

-- n  =>  a^e  (with e /= 1)
factorAsPower :: Rule Expr
factorAsPower = makeSimpleRuleList (power, "factor-as-power") $ \ expr -> do
  n      <- matchM myIntegerView expr
  (a, x) <- PF.allPowers $ toInteger n
  if n > 0
    then return $ fromInteger a .^. fromInteger x
    else if odd x
      then return $ fromInteger (negate a) .^. fromInteger x
      else fail "Could not factorise number."

calcPower :: Rule Expr 
calcPower = makeSimpleRule "arithmetic.operation.rational.power" $ \ expr -> do 
  (a, x) <- match (powerViewWith rationalView plainNatView) expr
  return $ fromRational $ a Prelude.^ x

-- | a^(x/y) => (a^x)^(1/y)
calcPowerRatio :: Rule Expr
calcPowerRatio = makeSimpleRule (power, "power-ratio") $ \ expr -> do
  (a, (x, y)) <- match (powerView >>> second divView) expr
  guard $ x /= 1 && y /= 1
  return $ (a .^. x) .^. (1 ./. y)

-- | root n x
calcRootHead :: Rule (OrList Expr)
calcRootHead = makeSimpleRule (power, "root") $ \ ors -> do
  expr   <- disjunctions ors >>= safeHead
  (n, x) <- match (rootView >>> (integerView *** integerView)) expr
  y      <- liftM fromInteger $ lookup n $ map swap $ PF.allPowers (abs x)
  let ys | x > 0 && even n = [y, negate y]
         | x > 0 && odd  n = [y]
         | x < 0 && odd  n = [negate y]
         | otherwise       = []
  roots  <- toMaybe (not. null) ys
  return $ orList roots

-- | [root n x, ... ]
calcRoot :: Rule (OrList Expr)
calcRoot = makeSimpleRuleList (power, "root") $ \ ors ->
  fromMaybe [] (disjunctions ors) >>= maybeToList . f
    where 
      f expr = do
        (n, x) <- match (rootView >>> (integerView *** integerView)) expr
        y      <- liftM fromInteger $ lookup n $ map swap $ PF.allPowers (abs x)
        let ys | x > 0 && even n = [y, negate y]
               | x > 0 && odd  n = [y]
               | x < 0 && odd  n = [negate y]
               | otherwise       = []
        roots  <- toMaybe (not. null) ys
        return $ orList roots

calcPowerPlus :: Rule Expr 
calcPowerPlus = 
  makeCommutative sumView (.+.) $ calcBinPowerRule "plus" (.+.) isPlus 

calcPowerMinus :: Rule Expr 
calcPowerMinus = 
   makeCommutative sumView (.+.) $ calcBinPowerRule "minus" (.-.) isMinus

addExponents :: Rule Expr
addExponents = makeSimpleRuleList (power, "add-exponents") $ \ expr -> do
  (sign, fs)     <- matchM (powerFactorView isPow) expr
  ((x, y), fill) <- twoNonAdjacentHoles fs
  prod           <- applyM addExponentsT $ x * y
  return $ build productView (sign, fill prod)

isPow :: Expr -> Expr -> Bool
isPow x y = x `belongsTo` myIntegerView && 
             (y `belongsTo` varView || y `belongsTo` powerView) 

-- | a*x^y * b*x^q = a*b * x^(y+q)
addExponentsT :: Transformation Expr 
addExponentsT = makeTrans $ \ expr -> do
  (e1, e2)     <- match timesView expr
  (a, (x,  y)) <- match unitPowerView e1
  (b, (x', q)) <- match unitPowerView e2
  guard $ x == x'
  return $ build unitPowerView (a .*. b, (x, y .+. q))

simpleAddExponents :: Rule Expr
simpleAddExponents = makeRule (power, "simple-add-exponents") addExponentsT

-- | a*x^y / b*x^q = a/b * x^(y-q)
subExponents :: Rule Expr
subExponents = makeSimpleRule (power, "sub-exponents") $ \ expr -> do
  (e1, e2)     <- match divView expr
  (a, (x,  y)) <- match unitPowerView e1
  (b, (x', q)) <- match unitPowerView e2
  guard $ x == x'
  return $ build unitPowerView (a ./. b, (x, y .-. q))

-- | (a^x)^y = a^(x*y)
mulExponents :: Rule Expr 
mulExponents = makeSimpleRule (power, "mul-exponents") $ \ expr -> do
  ((a, x), y) <- match (strictPowerView >>> first powerView) expr
  return $ build powerView (a, x .*. y)

-- | (a0 * a1 ... * an)^x = a0^x * a1^x ... * an^x
distributePower :: Rule Expr
distributePower = makeSimpleRule (power, "distr-power") $ \ expr -> do
  ((sign, as), x) <- match (powerViewWith productView identity) expr
  guard $ length as > 1
  let y = build productView (False, map (\a -> build powerView (a, x)) as)
  return $ 
    maybe y (\n -> if odd n && sign then neg y else y) $ match integerView x

-- | (a/b)^y = (a^y / b^y)
distributePowerDiv :: Rule Expr
distributePowerDiv = makeSimpleRule (power, "distr-power-div") $ \ expr -> do
  ((a, b), y) <- match (powerViewWith divView identity) expr
  return $ build divView (build powerView (a, y), build powerView (b, y))

-- | a^0 = 1
zeroPower :: Rule Expr
zeroPower = makeSimpleRule (power, "power-zero") $ \ expr -> do
  (_, x) <- match powerView expr
  guard $ x == 0
  return 1

-- a ^ 1 = a
onePower :: Rule Expr
onePower = makeSimpleRule (power, "power-one") $ \ expr -> do
  (a, x) <- match powerView expr
  guard $ x == 1
  return a

-- 1 ^ x = 1
powerOne :: Rule Expr
powerOne = makeSimpleRule (power, "one-power") $ \ expr -> do
  (a, _) <- match powerView expr
  guard $ a == 1
  return a

-- 0 ^ x = 0 with x > 0
powerZero :: Rule Expr
powerZero = makeSimpleRule (power, "one-power") $ \ expr -> do
  (a, x) <- match (powerViewWith identity integerView) expr
  guard $ x > 0 && a == 0
  return 0

-- | all of the above simplification rules
simplifyPower :: Rule Expr
simplifyPower = makeSimpleRuleList (power, "simplify") $ \ expr ->
  mapMaybe (`apply` expr) [zeroPower, onePower, powerOne, powerZero]

-- | e/a = e*a^(-1)  where a is an variable
reciprocalVar :: Rule Expr
reciprocalVar = makeSimpleRule (power, "reciprocal-var") $ \ expr -> do
  (e, (c, (a, x))) <- match (divView >>> second unitPowerViewVar) expr
  return $ (e .*. build unitPowerViewVar (1, (a, neg x))) ./. c

-- | c/a^x = c*a^x^(-1)
reciprocalPower :: Rule Expr
reciprocalPower = makeSimpleRule (power, "reciprocal-power") $ \ expr -> do
  (e, (c, (a, x))) <- match (divView >>> second consPowerView) expr
  return $ (e .*. build consPowerView (1, (a, neg x))) ./. c

-- | Use with care, will match any fraction!
reciprocal :: Rule Expr  
reciprocal = makeSimpleRule (power, "reciprocal") $
  apply (reciprocalForT identity)

-- | a/b = a*b^(-1)
reciprocalForT :: View Expr a -> Transformation Expr
reciprocalForT v = makeTrans $ \ expr -> do
  (a, b) <- match divView expr
  guard $ b `belongsTo` v
  return $ a .*. build powerView (b, -1)

-- | a^x = 1/a^(-x)
reciprocalInv ::  Rule Expr
reciprocalInv = makeSimpleRule (power, "reciprocal-inverse") $ \ expr -> do
  guard $ hasNegExp expr
  (a, x) <- match strictPowerView expr
  return $ 1 ./. build strictPowerView (a, neg x)

-- | c / d*a^(-x)*b^(-y)...p^r... = c*a^x*b^y.../d*p^r...
reciprocalFrac :: Rule Expr
reciprocalFrac = makeSimpleRule (power, "reciprocal-frac") $ \ expr -> do
  (e1, e2) <- match divView expr
  (s, xs)  <- match productView e2
  let (ys, zs) = partition hasNegExp xs
  guard (not $ null ys)
  return $ e1 .*. build productView (s, map f ys) ./. build productView (False, zs)
    where
      f e = case match consPowerView e of
              Just (c, (a, x)) -> build consPowerView (c, (a, neg x))
              Nothing          -> e

-- | a^x / b^x = (a/b)^x
divBase :: Rule Expr
divBase = describe "divide base of root" $
  makeSimpleRule (power, "divide-base") $ \ expr -> do
  (e1, e2)      <- match divView expr
  (c1, (a, x))  <- match consPowerView e1
  (c2, (b, x')) <- match consPowerView e2
  guard $ x == x' && b /= 0
  return $ build consPowerView (c1 .*. c2, (a ./. b, x))

-- | (-a)^x = -(a^x)
pushNegOut :: Rule Expr
pushNegOut = makeSimpleRule (power, "push-negation-out") $ \ expr -> do
  (a, x) <- match (powerViewWith identity integerView) expr
  a'     <- isNegate a
  return $ (if odd x then neg else id) $ build powerView (a', fromInteger x)


-- | Root rules ----------------------------------------------------------------

-- | a^(p/q) = root (a^p) q
power2root :: Rule Expr
power2root = makeSimpleRule (power, "write-as-root") $ \ expr -> do
  (a, (p, q)) <- match (strictPowerView >>> second divView) expr
  guard $ q /= 1
  return $ root (a .^. p) q
  
-- | root a q = a^(1/q)
root2power :: Rule Expr 
root2power = makeSimpleRule (power, "write-as-power") $ \ expr -> do
  (a, q) <- match strictRootView expr
  return $ a .^. (1 ./. q)


-- | Logarithmic relation rules -----------------------------------------------

logarithm :: Rule (Equation Expr)
logarithm = makeSimpleRule (logarithmic, "logarithm") $ \(lhs :==: rhs) -> do
    (b, x) <- match logView lhs
    return $ x :==: build powerView (b, rhs)


-- | Common rules --------------------------------------------------------------

-- | a/b * c/d = a*c / b*d  (b or else d may be one)  
myFractionTimes :: Rule Expr
myFractionTimes = smartRule $ makeSimpleRule (power, "fraction-times") $ \ expr -> do
  (e1, e2) <- match timesView expr
  guard $ e1 `belongsTo` divView || e2 `belongsTo` divView
  (a, b)   <- match (divView <&> (identity >>^ \e -> (e,1))) e1
  (c, d)   <- match (divView <&> (identity >>^ \e -> (e,1))) e2
  return $ build divView (a .*. c, b .*. d)


-- | Help functions -----------------------------------------------------------

calcBinPowerRule :: String -> (Expr -> Expr -> Expr) -> (Expr -> Maybe (Expr, Expr)) -> Rule Expr   
calcBinPowerRule opName op m = 
  makeSimpleRule (power, "calc-power", opName) $ \e -> do
    (e1, e2)     <- m e
    (c1, (a, x)) <- match unitPowerViewVar e1
    (c2, (b, y)) <- match unitPowerViewVar e2
    guard $ a == b && x == y
    return $ build unitPowerViewVar (op c1 c2, (a, x))

-- use twoNonAdHoles instead of split ???
makeCommutative :: View Expr [Expr] -> (Expr -> Expr -> Expr) -> Rule Expr -> Rule Expr
makeCommutative view op r = 
  makeSimpleRuleList (getId r) $ \ expr ->
    case match view expr of
      Just factors -> do
        (e, es) <- split op factors
        case apply r e of
          Just e' -> return $ build view (e' : es)
          Nothing -> []
      Nothing -> []

hasNegExp :: Expr -> Bool
hasNegExp expr = fromMaybe False $ 
  fmap ((< 0) . snd . snd) (match consPowerView expr)
