module Domain.Math.View.Numeric 
   ( integralView, realView
   , integerView, rationalView, doubleView
   ) where

import Common.View
import Control.Monad
import Data.Ratio
import Domain.Math.Expr.Symbolic
import Domain.Math.Expr.Symbols
import Domain.Math.Expr

-------------------------------------------------------------------
-- Numeric views

integralView :: Integral a => View Expr a
integralView = makeView (exprToNum f) fromIntegral
 where
   f s [x, y] | s == divSymbol = intDiv x y
   f _ _ = Nothing

realView :: RealFrac a => View Expr a
realView = makeView (exprToNum f) (fromRational . toRational)
 where
   f s [x, y] | s == divSymbol = fracDiv x y
   f _ _ = Nothing
   
integerView :: View Expr Integer
integerView = integralView

rationalView :: View Expr Rational
rationalView = makeView (match realView) fromRational

-- No floating view
doubleView :: View Expr Double
doubleView = makeView (exprToNum doubleSym)
                      (fromRational . flip approxRational 0.0001)
 
-------------------------------------------------------------------
-- Helper functions

doubleSym :: Symbol -> [Double] -> Maybe Double
doubleSym s [x, y] | s == divSymbol  = fracDiv x y
doubleSym s [x]    | s == sqrtSymbol && x >= 0 = Just (sqrt x)
doubleSym _ _ = Nothing

-- General numeric interpretation function: constructors Sqrt and
-- (:/:) are interpreted with function
exprToNum :: (Monad m, Num a) => (Symbol -> [a] -> m a) -> Expr -> m a
exprToNum f = foldExpr 
   ( liftM2 (+)
   , liftM2 (*)
   , liftM2 (-)
   , liftM negate
   , return . fromInteger
   , \mx my -> do x <- mx; y <- my; f divSymbol [x, y]
   , \mx    -> do x <- mx; f sqrtSymbol [x]
   , \_     -> fail "exprToNum: variable"
   , \s xs  -> sequence xs >>= f s
   )

intDiv :: Integral a => a -> a -> Maybe a
intDiv x y 
   | y /= 0 && m == 0 = Just d
   | otherwise        = Nothing
 where (d, m) = x `divMod` y
 
fracDiv :: Fractional a => a -> a -> Maybe a
fracDiv x y 
   | y /= 0    = Just (x / y)
   | otherwise = Nothing