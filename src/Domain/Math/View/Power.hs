-- For now, restricted to integers in exponent:
-- no sqrt, or roots
module Domain.Math.View.Power 
   ( powerView, powerViewFor
   , powerFactorView, powerFactorViewForWith
   ) where

import qualified Prelude
import Prelude hiding ((^), recip)
import Control.Monad
import Domain.Math.Expr
import Domain.Math.Expr.Symbols
import Domain.Math.View.Basic hiding (powerView)

----------------------------------------------------------------------
-- Simplified views (no side-conditions to worry about)

powerView :: View Expr (String, Int)
powerView = makeView f g
 where
   f expr = do
      pv <- selectVar expr
      n  <- match (powerViewFor pv) expr
      return (pv, n)
   g (pv, n) = build (powerViewFor pv) n

powerViewFor :: String -> View Expr Int
powerViewFor pv = makeView f g
 where
   f expr = 
      case expr of
         Var s | pv == s -> Just 1
         e1 :*: e2 -> liftM2 (+) (f e1) (f e2) 
         Sym s [e, Nat n] 
            | s == powerSymbol -> liftM (* fromInteger n) (f e)
         _ -> Nothing
   
   g a = Var pv .^. fromIntegral a

powerFactorView :: View Expr (String, Expr, Int)
powerFactorView = makeView f g
 where
   f expr = do
      pv <- selectVar expr
      (e, n) <- match (powerFactorViewForWith pv identity) expr
      return (pv, e, n)
   g (pv, e, n) = build (powerFactorViewForWith pv identity) (e, n)

powerFactorViewForWith :: Num a => String -> View Expr a -> View Expr (a, Int)
powerFactorViewForWith pv v = makeView f g
 where
   f expr = 
      case expr of
         Var s | pv == s -> Just (1, 1)
         Negate e -> do
            (a, b) <- f e
            return (negate a, b)
         e1 :*: e2 -> do 
            (a1, b1) <- f e1
            (a2, b2) <- f e2
            return (a1*a2, b1+b2)
         Sym s [e1, Nat n]
            | s == powerSymbol -> do 
                 (a1, b1) <- f e1
                 a <- match v (build v a1 ^ Nat n)
                 return (a, b1 * fromInteger n)
         _ -> do
            guard (pv `notElem` collectVars expr)
            a <- match v expr 
            return (a, 0)
   
   g (a, b) = build v a .*. (Var pv .^. fromIntegral b)

----------------------------------------------------------------------
-- General views (that have to cope with side-conditions)
{-
-- x^n
genPowerView :: Num a => String -> View Expr a -> View Expr a
genPowerView pv v = makeView f g
 where
   f expr = 
      case expr of
         Var s | pv == s -> Just 1
         e1 :*: e2 -> liftM2 (+) (f e1) (f e2)
         e1 :/: e2 -> liftM2 (-) (f e1) (f e2)    -- introduces a condition (silently)
         Sym s [e1, e2]                           -- e2 should not be negative
            | s == powerSymbol -> 
                 liftM2 (*) (f e1) (match v e2)
         _ -> Nothing
   
   g a = Var pv .^. build v a

-- a*x^n
genPowerFactorView :: (Fractional a, Num b) => 
                      String -> View Expr a -> View Expr b -> View Expr (a, b)
genPowerFactorView pv v1 v2 = makeView f g
 where
   f expr = 
      case expr of
         Var s | pv == s -> Just (1, 1)
         e1 :*: e2 -> do 
            (a1, b1) <- f e1
            (a2, b2) <- f e2
            return (a1*a2, b1+b2)
         e1 :/: e2 -> do     -- introduces a condition (silently)
            (a1, b1) <- f e1
            (a2, b2) <- f e2
            return (a1/a2, b1-b2)
         Sym s [e1, e2]      -- e2 should not be negative
            | s == powerSymbol -> do 
                 (a1, b1) <- f e1
                 n <- match v2 e2
                 a <- match v1 (build v1 a1 ^ build v2 n)
                 return (a, b1*n)
         _ -> do
            guard (pv `notElem` collectVars expr)
            a <- match v1 expr 
            return (a, 0)
   
   g (a, b) = build v1 a .*. (Var pv .^. build v2 b)
-}

{-
powerView :: Integral a => String -> View Expr a -> View Expr a
powerView = undefined

-- helper: also generalizes over number type in exponent (not just Int)
genPowerView :: (Num a, Num b) => String -> View Expr a -> View Expr b -> View Expr (a, b)
genPowerView = genPowerViewWith

genPowerViewWith :: (Num a, Num b) => String -> View Expr a -> View Expr b -> View Expr (a, b)
genPowerViewWith pv v1 v2 = makeView f g
 where
   f expr =
      case expr of
         Var s | pv == s -> Just (1, 1)
         e1 :*: e2 -> do 
            (a1, b1) <- f e1
            (a2, b2) <- f e2
            return (a1*a2, b1+b2)
         e1 :/: e2 -> do 
            (a1, b1) <- f e1
            (a2, b2) <- f e2
            a        <- match v1 (build v1 a1 / build v1 a2)
            return (a, b1-b2) 
         Sqrt e -> f (root e 2)
         Sym s [e1, e2] 
            | s == rootSymbol -> do
                 (a1, b1) <- f e1
                 n <- match v2 e2
                 a <- match v1 (build v1 a1 ^ build v2 n)
                 b <- match v2 (build v2 b1 / build v2 n)
                 return (a, b)
            | s == powerSymbol -> do 
                 (a1, b1) <- f e1
                 n <- match v2 e2
                 a <- match v1 (build v1 a1 ^ build v2 n)
                 return (a, b1*n)
         _ -> liftM (\a -> (a, 0)) (match v1 expr)
      
   g (a, b) = build v1 a .*. (Var pv .^. build v2 b)
   

test = match (genPowerView "x" identity integralView) (sqrt (Var "x" ^ 4)) -}