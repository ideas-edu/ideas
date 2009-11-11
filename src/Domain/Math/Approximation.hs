-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Selection of numerical algorithms for approximations
--
-----------------------------------------------------------------------------
module Domain.Math.Approximation where

import Data.List

type Function = Double -> Double

type Approximation = [Double]

------------------------------------------------------------
-- Precision of a floating-point number

precision :: Int -> Double -> Double
precision n = (/a) . fromIntegral . round . (*a)
 where a = 10 Prelude.^ (max 0 n)

------------------------------------------------------------
-- Stop criteria

within :: Double -> Approximation -> Double
within _ []  = error "within []"
within _ [x] = x
within d (x:xs@(y:_))
   | abs (x-y) <= d = x
   | otherwise      = within d xs

relative :: Double -> Approximation -> Double
relative _ []  = error "relative []"
relative _ [x] = x
relative d (x:xs@(y:_))
   | abs (x-y) <= d*abs y = x
   | otherwise            = relative d xs

------------------------------------------------------------
-- Root-finding algorithms

-- http://en.wikipedia.org/wiki/Bisection_method
bisection :: Function -> [Double] -> Approximation
bisection f ds = 
   case partition ((<= 0) . f) ds of
      (lo:_, hi:_) -> run hi lo
      _            -> []
 where
   run hi lo
      | fm <= 0   = mid : run hi mid
      | otherwise = mid : run mid lo
    where
      mid = (hi+lo) / 2
      fm  = f mid

-- http://en.wikipedia.org/wiki/Newton's_method
newton :: Function -> Function -> Double -> Approximation
newton f df x0 = iterate next x0
 where
    next a
       | dfa == 0  = a
       | otherwise = a - f a / dfa
     where
       dfa = df a

------------------------------------------------------------
-- Finding the derivative of a function
    
derivative :: Double -> Function -> Function
derivative delta f x = (f (x+delta) - f (x-delta)) / (2*delta)

-- Test code
{-
same f g = sum [ abs (f x - g x) | x <- [0,0.01..6] ]

test1 = same (derivative 0.01 sin) cos
test2 = same (derivative 0.01 cos) (negate . sin) -}