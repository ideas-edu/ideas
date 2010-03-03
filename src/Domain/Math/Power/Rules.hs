-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Domain.Math.Power.Rules where

import Prelude hiding ( (^) )
import qualified Prelude
import Common.Apply
import Control.Arrow ( (>>^) )
import Common.Transformation
import Common.View
import Control.Monad
import Data.List
import Data.Maybe
import Domain.Math.Expr
import Domain.Math.Expr.Symbols
import Domain.Math.Numeric.Rules
import Domain.Math.Numeric.Views
import Domain.Math.Power.Views

------------------------------------------------------------
-- Rules
smartRule :: Rule Expr -> Rule Expr
smartRule = doAfter f
  where
    f (a :*: b) = a .*. b
    f (a :/: b) = a ./. b
    f (Negate a) = neg a
    f (a :+: b) = a .+. b
    f (a :-: b) = a .-. b
    f e = e

-- | The rules
powerRules =
   [ addExponents
   , subExponents
   , mulExponents
   , distributePower
   , zeroPower
   , reciprocal
   , fracExponent
   , calcPower
   , calcBinPowerRule "minus" (-) isMinus
   , calcBinPowerRule "plus" (+) isPlus
   , myFractionTimes
   ]
   
calcBinPowerRule opName op m = 
   makeSimpleRule ("calculate power " ++ opName) $ \e -> do
     (e1, e2) <- m e
     (c1, a)  <- match strictPowerView e1
     (c2, b)  <- match strictPowerView e2
     guard (a == b)
     return (build strictPowerView ((op c1 c2), a))

calcPower :: Rule Expr 
calcPower = makeSimpleRule "calculate power" $ \ expr -> do 
  (e1, e2) <- match simplePowerView expr
  a        <- match rationalView e1
  x        <- match integralView e2
  guard (x > 0)  
  return $ fromRational $ a Prelude.^ x

-- | a*x^y * b*x^q = a*b * x^(y+q)
addExponents :: Rule Expr 
addExponents = makeSimpleRuleList "add exponents" $ \ expr -> do
  case match (powerFactorisationView unitPowerView) expr of
    Just (s, fs) -> do 
      (e, es) <- split fs
      case apply addExponents' e of
        Just e' -> return $ build productView (s, e' : es)
        Nothing -> fail ""  
    Nothing -> fail ""

split :: [Expr] -> [(Expr, [Expr])]
split xs = f xs
  where
    f (y:ys) | not (null ys) = [(y * z, xs \\ [y, z]) | z <- ys] ++ f ys 
             | otherwise     = []
    f [] = []

-- | a*x^y * b*x^q = a*b * x^(y+q)
addExponents' :: Rule Expr 
addExponents' = makeSimpleRule "add exponents" $ \ expr -> do
  x        <- selectVar expr
  (e1, e2) <- match timesView expr
  (a, y)   <- match (unitPowerForView x) e1
  (b, q)   <- match (unitPowerForView x) e2
  return $ build (unitPowerForView x) (a .*. b, y + q)
  
-- | a*x^y / b*x^q = a/b * x^(y-q)
subExponents :: Rule Expr
subExponents = makeSimpleRule "sub exponents" $ \ expr -> do
  x        <- selectVar expr
  (e1, e2) <- match divView expr
  (a, y)   <- match (unitPowerForView x) e1
  (b, q)   <- match (unitPowerForView x) e2
  return $ build (unitPowerForView x) (a ./. b, y - q)

-- | (c*a^x)^y = c*a^(x*y)
mulExponents :: Rule Expr 
mulExponents = makeSimpleRule "mul exponents" $ \ expr -> do
  (cax, y)    <- match simplePowerView expr
  (c, (a, x)) <- match strictPowerView cax
  guard (c == 1 || c == -1)
  a'      <- selectVar a
  return $ build strictPowerView (c, (a, x .*. y))

-- | c*(a0..an)^y = c * a0^y * a1^y .. * an^y
distributePower :: Rule Expr
distributePower = makeSimpleRule "distribute power" $ \ expr -> do
  (c, (as', y)) <- match strictPowerView expr
  y'            <- match integerView y
  (sign, as)    <- match productView as'
  guard (length as > 1)
  return $ build productView 
   (if sign then odd y' else False, c : map (\a -> a .^. y) as)

-- | c*a^0 = c
zeroPower :: Rule Expr
zeroPower = makeSimpleRule "zero power" $ \ expr -> do
  (_, (c, y)) <- match strictPowerView expr
  y' <- match integerView y
  guard (y'==0)
  return c

-- | d/c*a^x = d*a^(-x)/c
reciprocal :: Rule Expr
reciprocal = makeSimpleRule "reciprocal" $ \ expr -> do
  a        <- selectVar expr
  (d, cax) <- match divView expr
  (c, x)   <- match (unitPowerForView a) cax
  return $ build (unitPowerForView a) (d ./. c, negate x)

-- | c*a^x = c/a^(-x)
reciprocal' :: (Expr -> Bool) -> Rule Expr
reciprocal' p = makeSimpleRule "reciprocal" $ \ expr -> do
  guard (p expr)
  a        <- selectVar expr
  (c, x)   <- match (unitPowerForView a) expr
  return $ c ./. build (unitPowerForView a) (1, negate x)

-- | c*a^(p/q) = c * root q (a^p)
fracExponent :: Rule Expr 
fracExponent = makeSimpleRule "add exponents" $ \ expr -> do
  a             <- selectVar expr
  (a', (c, pq)) <- match strictPowerView expr
  guard (Var a == a')
  (p, q)        <- match divView pq
  return $ c .*. (root q (build strictPowerView (a', (1, p))))
  
-- | a/b * c/d = a*c / b*d  (b or else d may be one)  
myFractionTimes :: Rule Expr
myFractionTimes = smartRule $ makeSimpleRule "fraction times" $ \ expr -> do
  (e1, e2) <- match timesView expr
  guard $ isJust $ match divView e1 `mplus` match divView e2
  (a, b)   <- match (divView <&> (identity >>^ \e -> (e,1))) e1
  (c, d)   <- match (divView <&> (identity >>^ \e -> (e,1))) e2
  return $ build divView (a .*. c, b .*. d)

