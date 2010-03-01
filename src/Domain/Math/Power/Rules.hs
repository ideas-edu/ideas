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
import Common.Transformation
import Common.View
import Control.Monad
import Data.List
import Data.Maybe
import Domain.Math.Expr
import Domain.Math.Expr.Symbols
import Domain.Math.Numeric.Views
import Domain.Math.Power.Views

--import Data.Generics.Uniplate
import Data.Generics.PlateData

------------------------------------------------------------
-- Rules

-- | a*x^y * b*x^q = a*b * x^(y+q)
addExponents' :: Rule Expr 
addExponents' = makeSimpleRuleList "add exponents" $ \ expr -> do
--  pv <- collectVars expr
  case match (powerFactorisationView myPowerView) expr of
    Just (s, fs) -> do 
      (e, es) <- split fs
      case apply addExponents e of
        Just e' -> return $ build productView (s, e' : es)
        Nothing -> fail ""  
    Nothing -> fail ""
  
    
g :: [(Expr, [Expr])] -> [Expr]
g = map (\(x, xs) -> build productView (False, (x : xs)))

split :: [Expr] -> [(Expr, [Expr])]
split xs = f xs
  where
    f (y:ys) | not (null ys) = [(y * z, xs \\ [y, z]) | z <- ys] ++ f ys 
             | otherwise     = []
    f [] = []
  
  
-- | Test stuff
a = Var "a" ; b = Var "b"
expr = 2*3*a^6 * 5*6*a^7 * a^3 * b^2 * b^3

-- | The rules
powerRules =
   [ addExponents
   , addExponents'
   , subExponents
   , mulExponents
   , distributePower
   , zeroPower
   , reciprocal
   , fracExponent
   , calcPower
   ]

calcPower :: Rule Expr 
calcPower = minorRule $ makeSimpleRule "calculate power" $ \ expr -> do 
  (e1, e2) <- match plainPowerView expr
  x        <- match integralView e1
  y        <- match integralView e2
  return $ Nat $ x Prelude.^ y

-- | a*x^y * b*x^q = a*b * x^(y+q)
addExponents :: Rule Expr 
addExponents = makeSimpleRule "add exponents" $ \ expr -> do
  x        <- selectVar expr
  (e1, e2) <- match timesView expr
  (a, y)   <- match (myPowerForView x) e1
  (b, q)   <- match (myPowerForView x) e2
  return $ build (myPowerForView x) (a .*. b, y + q)
  
-- | a*x^y / b*x^q = a/b * x^(y-q)
subExponents :: Rule Expr
subExponents = makeSimpleRule "sub exponents" $ \ expr -> do
  x        <- selectVar expr
  (e1, e2) <- match divView expr
  (a, y)   <- match (myPowerForView x) e1
  (b, q)   <- match (myPowerForView x) e2
  return $ build (myPowerForView x) (a ./. b, y - q)

-- | d*(c*a^x)^y = d*c*a^(x*y)
mulExponents :: Rule Expr 
mulExponents = makeSimpleRule "mul exponents" $ \ expr -> do
  (d, (cax, y)) <- match (myPlainPowerView) expr
  (c, (a, x))   <- match (myPlainPowerView) cax
  a'            <- selectVar a  
  return $ build myPlainPowerView (d .*. c, (a, x .*. y))

-- | c*(a0..an)^y = c*a0^y * c*a1^y .. * c*an^y
distributePower :: Rule Expr
distributePower = makeSimpleRule "distribute power" $ \ expr -> do
  (c, (as', y)) <- match myPlainPowerView expr
  (sign, as)    <- match productView as'
  guard (length as > 1)
  return $ build productView 
    (sign, map (\a -> build myPlainPowerView (c, (a, y))) as)

-- | c*a^0 = c
zeroPower :: Rule Expr
zeroPower = makeSimpleRule "zero power" $ \ expr -> do
  (_, (c, y)) <- match myPowerView expr
  guard (y==0)
  return c

-- | d/c*a^x = d*a^(-x)/c
reciprocal :: Rule Expr
reciprocal = makeSimpleRule "reciprocal" $ \ expr -> do
  a        <- selectVar expr
  (d, cax) <- match divView expr
  (c, x)   <- match (myPowerForView a) cax
  return $ build (myPowerForView a) (d ./. c, negate x)

-- | c*a^(p/q) = c * root q (a^p)
fracExponent :: Rule Expr 
fracExponent = makeSimpleRule "add exponents" $ \ expr -> do
  a             <- selectVar expr
  (a', (c, pq)) <- match myPlainPowerView expr
  guard (Var a == a')
  (p, q)        <- match divView pq
  return $ c .*. (root q (build myPlainPowerView (a', (1, p))))