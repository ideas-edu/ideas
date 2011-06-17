-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Domain.Math.SquareRoot.Views
   ( squareRootView, squareRootViewWith
   ) where

import Control.Monad
import Common.View
import Domain.Math.Numeric.Views
import Domain.Math.Expr hiding ((^))
import Domain.Math.Data.SquareRoot

squareRootView :: View Expr (SquareRoot Expr)
squareRootView = squareRootViewWith identity

squareRootViewWith :: Fractional a => View Expr a -> View Expr (SquareRoot a)
squareRootViewWith v = makeView f g
 where
   f expr =
      case expr of
         Nat a    -> Just (fromIntegral a)
         a :+: b  -> liftM2 (+) (f a) (f b)
         a :-: b  -> liftM2 (-) (f a) (f b)
         Negate a -> fmap negate (f a)
         a :*: b  -> liftM2 (*) (f a) (f b)
         a :/: b  -> join $ liftM2 fracDiv (f a) (f b)
         Sqrt a   -> fmap sqrtRational (match rationalView a)
         Sym s [a, b] | isPowerSymbol s ->
            liftM2 power (f a) (match integerView b)
         _ -> fmap con (match v expr)
   
   power a n 
      | n >= 0    = a ^ n
      | otherwise = 1 / (a ^ abs n)
   
   g = to sumView . map h . toList
   h (a, n)  
      | n == 0    = 0
      | n == 1    = build v a
      | otherwise = build v a .*. Sqrt (fromIntegral n)