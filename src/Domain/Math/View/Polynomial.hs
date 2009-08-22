module Domain.Math.View.Polynomial 
   ( polyView, polyViewFor, polyViewWith, polyViewForWith
   ) where

import Control.Monad
import Data.List
import Domain.Math.Data.Polynomial
import Domain.Math.Expr
import Domain.Math.Expr.Symbols
import Domain.Math.View.Basic

polyView :: View Expr (String, Polynomial Expr)
polyView = polyViewWith identity

polyViewFor :: String -> View Expr (Polynomial Expr)
polyViewFor v = polyViewForWith v identity

polyViewWith :: Fractional a => View Expr a -> View Expr (String, Polynomial a)
polyViewWith v = makeView f (uncurry g)
 where
   f expr = do 
      pv <- selectVar expr
      p  <- match (polyViewForWith pv v) expr
      return (pv, p) 
   g pv = build (polyViewForWith pv v)
            
polyViewForWith :: Fractional a => String -> View Expr a -> View Expr (Polynomial a)
polyViewForWith pv v = makeView f g
 where
   f expr = 
      case expr of
         Var s | pv == s -> Just var
         Nat n    -> Just (fromIntegral n)
         Negate a -> liftM negate (f a)
         a :+: b  -> liftM2 (+) (f a) (f b)
         a :-: b  -> liftM2 (-) (f a) (f b)
         a :*: b  -> liftM2 (*) (f a) (f b)
         a :/: b  -> do
            c <- match v b
            guard (c /= 0)
            guard (pv `notElem` collectVars b)
            p <- f a
            return (fmap (/c) p)
         Sym s [a, n] | s == powerSymbol ->
           liftM2 power (f a) (match integralView n)
         _ -> do 
            guard (pv `notElem` collectVars expr)
            liftM con (match v expr)
   
   g        = build sumView . map h . terms
   h (n, a) = build v a .*. (Var pv .^. fromIntegral n)

-- helper
selectVar :: Expr -> Maybe String
selectVar = f . nub . collectVars
 where
   f []  = Just "x" -- exceptional case (a polynomial can be constant)
   f [a] = Just a
   f _   = Nothing