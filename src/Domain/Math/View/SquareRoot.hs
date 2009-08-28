module Domain.Math.View.SquareRoot 
   ( squareRootView, squareRootViewWith
   ) where

import Control.Monad
import Domain.Math.Expr
import Domain.Math.Data.SquareRoot
import Domain.Math.Expr.Symbols hiding ((^))
import Domain.Math.View.Basic

squareRootView :: View Expr (SquareRoot Rational)
squareRootView = squareRootViewWith rationalView

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
         Sym s [a, b] | s == powerSymbol ->
            liftM2 (^) (f a) (match integerView b)
         _ -> fmap con (match v expr)
   
   g = build sumView . map h . toList
   h (a, n)  
      | n == 0    = 0
      | n == 1    = build v a
      | otherwise = build v a .*. Sqrt (fromIntegral n)