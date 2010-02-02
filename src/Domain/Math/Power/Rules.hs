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

import Common.Transformation
import Common.View
import Control.Monad
import Data.List
import Data.Maybe
import Domain.Math.Expr
import Domain.Math.Expr.Symbols
import Domain.Math.Numeric.Views
import Domain.Math.Power.Views


------------------------------------------------------------
-- Rules

-- | a*x^y * b*x^q = a*b * x^(y+q)
addExponents :: Rule Expr 
addExponents = makeSimpleRule "add exponents" $ \ expr -> do
  (e1, e2)     <- match timesView expr
  (a, (x, y))  <- match myPowerView e1
  (b, (x', q)) <- match myPowerView e2
  guard (x == x')
  return $ build myPowerView (a .*. b, (x, y .+. q))

-- | a*x^y / b*x^q = a/b * x^(y-q)
subExponents :: Rule Expr
subExponents = makeSimpleRule "sub exponents" $ \ expr -> do
  (e1, e2)     <- match divView expr
  (a, (x, y))  <- match myPowerView e1
  (b, (x', q)) <- match myPowerView e2
  guard (x == x')
  return $ build myPowerView (a ./. b, (x, y .-. q))

-- | d*(c*a^x)^y = d*c*a^(x*y)
mulExponents :: Rule Expr 
mulExponents = makeSimpleRule "add exponents" $ \ expr -> do
  (d, (ax, y)) <- match myPowerView expr
  (c, (a, x))  <- match myPowerView ax
  return $ build myPowerView (d .*. c, (a, x .*. y))

-- | c*(a0..an)^y = c*a0^y * c*a1^y .. * c*an^y
distributePower :: Rule Expr
distributePower = makeSimpleRule "distribute power" $ \ expr -> do
  (c, (as', y)) <- match myPowerView expr
  (sign, as)    <- match productView as'
  guard (length as > 1)
  return $ build productView 
    (sign, map (\a -> build myPowerView (c, (a, y))) as)

-- | c*a^0 = c
zeroPower :: Rule Expr
zeroPower = makeSimpleRule "zero power" $ \ expr -> do
  (c, (a, y)) <- match myPowerView expr
  guard (y==0)
  return c

-- | d/c*a^x = d*c*a^(-x)
reciprocal :: Rule Expr
reciprocal = makeSimpleRule "reciprocal" $ \ expr -> do
  (d, cax)    <- match divView expr
  (c, (a, x)) <- match myPowerView cax
  return $ build myPowerView (d .*. c, (a, negate x))

-- | c*a^(p/q) = c * root q (a^p)
fracExponent :: Rule Expr 
fracExponent = makeSimpleRule "add exponents" $ \ expr -> do
  (c, (a, (p, q))) <- match powerConsDivView expr
  return $ c .*. (root q (build powerConsDivView (1, (a, (p, 1)))))
  
  