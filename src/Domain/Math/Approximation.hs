-- Selection of numerical algorithms for approximations
module Domain.Math.Approximation where

import Data.List

type Function = Double -> Double

type Approximation = [Double]

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