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
-----------------------------------------------------------------------------
-- For now, restricted to integers in exponent:
-- no sqrt, or roots
module Domain.Math.Power.Views where

import qualified Prelude
import Prelude hiding ((^), recip)
import Control.Arrow ( (>>^) )
import Control.Monad
import Common.View
import Data.List
import Data.Maybe
import Data.Ratio
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import Domain.Math.Expr.Views

-- | Combined power views
(<&>) :: (MonadPlus m) => ViewM m a b -> ViewM m a b -> ViewM m a b
v <&> w = makeView f g
  where
    f x = match v x `mplus` match w x
    g   = build v
infixl <&>

strictPowerView :: View Expr (Expr, (Expr, Expr))
strictPowerView  =  strictPowerConsView 
                <&> (simplePowerView >>^ (,) 1) 
                <&> negPowerView
  where
    strictPowerConsView = timesView >>> second simplePowerView
    negPowerView = makeView f g
      where
        f (Negate expr) = do 
          (c, ax) <- match (strictPowerConsView <&> (simplePowerView >>^ (,) 1)) expr
          return (negate c, ax)
        f _ = Nothing
        g = build strictPowerView        

strictPowerViewFor :: String -> View Expr (Expr, Expr)
strictPowerViewFor pv = makeView f g
  where
    f expr = do
      (c, (a, x)) <- match strictPowerView expr
      guard (Var pv == a)
      return (c, x)
    g (c, x) = build strictPowerView (c, (Var pv, x))

powerConsViewFor :: String -> View Expr (Expr, Rational)
powerConsViewFor pv = timesView >>> second (powerViewFor' pv)

powerConsView :: View Expr (String, (Expr, Rational))
powerConsView = makeView f g
  where
    f expr = do
      pv <- selectVar expr
      cn <- match (powerConsViewFor pv) expr
      return (pv, cn)
    g (pv, cn) = build (powerConsViewFor pv) cn
    
unitPowerForView :: String -> ViewM Maybe Expr (Expr, Rational)
unitPowerForView pv = powerConsViewFor pv <&> (powerViewFor' pv >>^ (,) 1)

unitPowerView :: ViewM Maybe Expr (String, (Expr, Rational))
unitPowerView = unitView <&> negUnitView 
  where 
    unitView = powerConsView <&> (powerView' >>^ \(pv, n) -> (pv, (1, n))) 
    negUnitView = makeView f g
      where
        f (Negate expr) = do 
          (a, (c, x)) <- match unitView expr
          return (a, (negate c, x))
        f _ = Nothing
        g = build unitView      

-- | Power views
simplePowerView :: View Expr (Expr, Expr)
simplePowerView = makeView f g
  where
    f expr = 
      case expr of
        Sym s [a, b] | s == powerSymbol -> return (a, b)
        _ -> Nothing
    g (a, b) = a .^. b   

powerFactorisationView :: View Expr a -> View Expr (Bool, [Expr])
powerFactorisationView v = productView >>> second (makeView f id)
  where
    f es = return $ map (\x -> build productView (False, x)) $ factorise es
    factorise :: [Expr] -> [[Expr]]
    factorise es =  maybe [es] split $ findIndex isPower es
      where
        split i = let (xs, ys) = splitAt (i+1) es in xs : factorise ys
        isPower = isJust . match v

rootView :: View Expr (Expr, Rational)
rootView = makeView f g
  where 
    f expr = case expr of
        Sqrt e -> return (e, 2)
        Sym s [a, Nat b] | s == rootSymbol -> return (a, toRational b)
        _ -> Nothing
    g (a, b) = if b==2 then Sqrt a else root a (fromRational b)

----------------------------------------------------------------------
-- Simplified views (no side-conditions to worry about)

natView = makeView f fromIntegral
  where
    f (Nat n) = Just $ fromInteger n
    f _       = Nothing

intView :: View Expr Int
intView = makeView f fromIntegral
  where
    f (Nat n) = Just $ fromInteger n
    f (Negate e) = do
      e' <- f e
      return $ negate e'
    f _       = Nothing

ratioView :: View Rational (Int, Int)
ratioView = makeView f g
  where
    f x = return (fromIntegral (numerator x), fromIntegral (denominator x))
    g (n,d) = (fromIntegral n) % (fromIntegral d)

powerView :: View Expr (String, Int)
powerView = powerViewWith natView
powerView' = powerViewWith' rationalView
   
powerViewWith :: Num a => View Expr a -> View Expr (String, a)
powerViewWith v = makeView f g
 where
   f expr = do
      pv <- selectVar expr
      n  <- match (powerViewForWith v pv) expr
      return (pv, n)
   g (pv, n) = build (powerViewForWith v pv) n

powerViewWith' v = makeView f g
 where  
   f expr = do
      pv <- selectVar expr
      n  <- match (powerViewForWith' v pv) expr
      return (pv, n)
   g (pv, n) = build (powerViewForWith' v pv) n
   
powerViewFor :: String -> View Expr Int
powerViewFor = powerViewForWith natView
powerViewFor' = powerViewForWith' rationalView

powerViewForWith :: Num a =>  View Expr a -> String -> View Expr a
powerViewForWith v pv = makeView f g
 where
   f expr = 
      case expr of
         Var s | pv == s -> match v 1
         e1 :*: e2 -> liftM2 (+) (f e1) (f e2) 
         Sym s [e, n] | s == powerSymbol -> do
           n'<- match v n
           liftM (* n') (f e)
         _ -> Nothing
   
   g a = Var pv .^. build v a
   
powerViewForWith' v pv = makeView f g
 where
   f expr = 
      case expr of
        Var s | pv == s -> match v 1
        Sym s [Var s', n] | s' == pv && s == powerSymbol -> do
          match v n
        _ -> Nothing
   
   g a = Var pv .^. build v a

powerFactorView :: View Expr (String, Expr, Int)
powerFactorView = powerFactorViewWith identity

powerFactorViewWith :: Num a => View Expr a -> View Expr (String, a, Int)
powerFactorViewWith v = makeView f g
 where
   f expr = do
      pv <- selectVar expr
      (e, n) <- match (powerFactorViewForWith pv v) expr
      return (pv, e, n)
   g (pv, e, n) = build (powerFactorViewForWith pv v) (e, n)

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



powerView :: Integral a => String -> View Expr a -> View Expr a
powerView = undefined

-- helper: also generalizes over number type in exponent (not just Int)
--genPowerView :: (Num a, Num b) => String -> View Expr a -> View Expr b -> View Expr (a, b)
--genPowerView = genPowerViewWith

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
-}   

--test = match (genPowerView "x" identity integralView) (sqrt (Var "x" ^ 4))