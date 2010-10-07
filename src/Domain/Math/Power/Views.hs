-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------

module Domain.Math.Power.Views 
   ( -- * Power views
     strictPowerView, strictPowerViewFor, powerConsViewFor, powerConsView
   , unitPowerView, unitPowerForView, simplePowerView, powerFactorisationView
   , powerFactorViewWith, powerViewFor', powerFactorViewForWith
   , powerViewFor, powerFactorView
     -- * Root views
   , rootView, rootConsView, simpleRootView
     -- * View combinator
   , (<&>)
     -- * Other views
   , ratioView, natView
   ) where

import Prelude hiding ((^), recip)
import Control.Arrow ( (>>^) )
import Control.Monad
import Common.View
import Data.List
import Data.Maybe
import Data.Ratio
import Domain.Math.Expr
import Domain.Math.Numeric.Views

-- | Combinator function
(<&>) :: (MonadPlus m) => ViewM m a b -> ViewM m a b -> ViewM m a b
v <&> w = makeView f g
  where
    f x = match v x `mplus` match w x
    g   = build v
infixl <&>


-- | Power views --------------------------------------------------------------
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
    unitView = powerConsView <&> (powerView >>^ \(pv, n) -> (pv, (1, n))) 
    negUnitView = makeView f g
      where
        f (Negate expr) = do 
          (a, (c, x)) <- match unitView expr
          return (a, (negate c, x))
        f _ = Nothing
        g = build unitView      

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

-- | Root views ---------------------------------------------------------------

simpleRootView :: View Expr (Expr, Expr)
simpleRootView = makeView f g
  where 
    f expr = case expr of
        Sqrt e -> return (e, 2)
        Sym s [a, b] | s == rootSymbol -> return (a, b)
        _ -> Nothing
    g (a, b) = if b == Nat 2 then Sqrt a else root a b

rootView = simpleRootView >>> second rationalView

-- rootView :: View Expr (Expr, Rational)
-- rootView = makeView f g
--   where 
--     f expr = case expr of
--         Sqrt e -> return (e, 2)
--         Sym s [a, Nat b] | s == rootSymbol -> return (a, toRational b)
--         _ -> Nothing
--     g (a, b) = if b==2 then Sqrt a else root a (fromRational b)

rootConsView :: View Expr (Expr, (Expr, Rational))
rootConsView =  (timesView >>> second rootView)
            <&> (rootView >>^ (,) 1)


-- | Bastiaan's power views ---------------------------------------------------

-- | AG: todo: integrate these views with the views above

natView :: View Expr Int
natView = makeView f fromIntegral
  where
    f (Nat n) = Just $ fromInteger n
    f _       = Nothing

ratioView :: View Rational (Int, Int)
ratioView = makeView f g
  where
    f x = return (fromIntegral (numerator x), fromIntegral (denominator x))
    g (n,d) = fromIntegral n % fromIntegral d

powerView :: View Expr (String, Rational)
powerView = powerViewWith rationalView

powerViewWith :: View Expr b -> View Expr (String, b)
powerViewWith v = makeView f g
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
        Sym s [Var s', n] | s' == pv && s == powerSymbol -> match v n
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
                 a <- match v (build v a1 ^ fromInteger n)
                 return (a, b1 * fromInteger n)
         _ -> do
            guard (pv `notElem` collectVars expr)
            a <- match v expr 
            return (a, 0)
   
   g (a, b) = build v a .*. (Var pv .^. fromIntegral b)
