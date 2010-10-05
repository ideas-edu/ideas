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
     -- * Normalising views
   , normPowerView, normPowerView', normPowerNonNegRatio
   , normPowerNonNegDouble, normPowerEqApproxView, normPowerEqView
     -- * Other views
   , ratioView, natView
   ) where

import Prelude hiding ((^), recip)
import qualified Prelude
import Control.Arrow ( (>>^) )
import Control.Monad
import Common.View
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ratio
import Domain.Math.Approximation (precision)
import Domain.Math.Data.Relation
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


-- | Normalising views ---------------------------------------------------------

normPowerNonNegRatio :: View Expr (M.Map String Rational, Rational) -- (Rational, M.Map String Rational)
normPowerNonNegRatio = makeView (liftM swap . f) (g . swap)
 where
     swap (x,y) = (y,x)
     f expr = 
        case expr of
           Sym s [a,b] 
              | s==powerSymbol -> do
                   (r, m) <- f a
                   if r==1 
                     then do
                       r2 <- match rationalView b
                       return (1, M.map (*r2) m)
                     else do
                       n <- match integerView b
                       if n >=0 
                         then return (r Prelude.^ n, M.map (*fromIntegral n) m)
                         else return (1/(r Prelude.^ abs n), M.map (*fromIntegral n) m)
              | s==rootSymbol ->
                  f (Sym powerSymbol [a, 1/b])
           Sqrt a -> 
              f (Sym rootSymbol [a,2])
           a :*: b -> do
             (r1, m1) <- f a
             (r2, m2) <- f b
             return (r1*r2, M.unionWith (+) m1 m2)
           a :/: b -> do
             (r1, m1) <- f a
             (r2, m2) <- f b
             guard (r2 /= 0)
             return (r1/r2, M.unionWith (+) m1 (M.map negate m2))
           Var s -> return (1, M.singleton s 1)
           Nat n -> return (toRational n, M.empty)
           Negate x -> do 
             (r, m) <- f x
             return (negate r, m)
           _ -> do
             r <- match rationalView expr
             return (fromRational r, M.empty)
     g (r, m) = 
       let xs = map f (M.toList m)
           f (s, r) = Var s .^. fromRational r
       in build productView (False, fromRational r : xs)

-- | AG: todo: change double to norm view for rationals
normPowerNonNegDouble :: View Expr (Double, M.Map String Rational)
normPowerNonNegDouble = makeView (liftM (roundof 6) . f) g
  where
    roundof n (x, m) = (fromIntegral (round (x * 10.0 ** n)) / 10.0 ** n, m)
    f expr = 
      case expr of
        Sym s [a,b] 
          | s==powerSymbol -> do
            (x, m) <- f a
            y      <- match rationalView b
            return (x ** fromRational y, M.map (*y) m)
          | s==rootSymbol -> f (Sym powerSymbol [a, 1/b])
        Sqrt a -> f (Sym rootSymbol [a,2])
        a :*: b -> do
          (r1, m1) <- f a
          (r2, m2) <- f b
          return (r1*r2, M.unionWith (+) m1 m2)
        a :/: b -> do
          (r1, m1) <- f a
          (r2, m2) <- f b
          guard (r2 /= 0)
          return (r1/r2, M.unionWith (+) m1 (M.map negate m2))
        Var s -> return (1, M.singleton s 1)
        Negate x -> do 
          (r, m) <- f x
          return (negate r, m)
        _ -> do
          d <- match doubleView expr
          return (d, M.empty)
    g (r, m) = 
      let xs = map f (M.toList m)
          f (s, r) = Var s .^. fromRational r
      in build productView (False, fromDouble r : xs)


type PowerMap = (M.Map String Rational, Rational)

normPowerView' :: View Expr [PowerMap]
normPowerView' = makeView (liftM h . f) g
  where
    f = (mapM (match normPowerNonNegRatio) =<<) . match sumView
    g = build sumView . map (build normPowerNonNegRatio)
    h :: [PowerMap] -> [PowerMap]
    h = map (foldr1 (\(x,y) (_,q) -> (x,y+q))) . groupBy (\x y -> fst x == fst y) . sort

normPowerView :: View Expr (String, Rational)
normPowerView = makeView f g
 where
   f expr = 
        case expr of
           Sym s [x,y] 
              | s==powerSymbol -> do
                   (s, r) <- f x
                   r2 <- match rationalView y
                   return (s, r*r2)
              | s==rootSymbol -> 
                   f (x^(1/y))
           Sqrt x ->
              f (Sym rootSymbol [x, 2])
           Var s -> return (s, 1) 
           x :*: y -> do
             (s1, r1) <- f x
             (s2, r2) <- f y
             guard (s1==s2)
             return (s1, r1+r2)
           Nat 1 :/: y -> do
             (s, r) <- f y
             return (s, -r)
           x :/: y -> do
             (s1, r1) <- f x
             (s2, r2) <- f y
             guard (s1==s2)
             return (s1, r1-r2) 
           _ -> Nothing
             
   g (s, r) = Var s .^. fromRational r

normPowerEqApproxView :: Int -> View (Relation Expr) (Expr, Expr)
normPowerEqApproxView d = makeView f (uncurry (.~=.))
   where
     f rel = case relationType rel of 
      EqualTo       ->  match (equationView >>> normPowerEqView) rel 
                    >>= return . \(l, r) -> (l, simplifyWith (precision d) doubleView r)
      Approximately -> return (leftHandSide rel, rightHandSide rel)

normPowerEqView :: View (Equation Expr) (Expr, Expr)
normPowerEqView = makeView f (uncurry (:==:))
  where
    f (lhs :==: rhs) = do
      v            <- selectVar (lhs .-. rhs)
      -- selected var to the left, the rest to the right
      (lhs', rhs') <- varLConR v lhs rhs
      -- match power
      (c, ax)      <- match (timesView <&> (identity >>^ (,) 1)) $
                        simplify normPowerView lhs'
      (a, x)       <- match myPowerView ax
      -- simplify, scale and take root
      return (a, simplify rationalView ((rhs' ./. c) .^. (1 ./. x)))

    myPowerView =  simplePowerView 
               <&> (simpleRootView >>> second (makeView (\a->Just (1 ./. a)) (\b->1 ./. b)))
               <&> (identity >>^ \a->(a,1))

varLConR v lhs rhs = do
  (xs, cs) <- match sumView lhs >>= return . partition (elem v . collectVars)
  (ys, ds) <- match sumView rhs >>= return . partition (elem v . collectVars)
  return ( build sumView (xs ++ map neg ys)
         , build sumView (ds ++ map neg cs) )
   
normExpEqView :: View (Equation Expr) (Expr, Expr)
normExpEqView = makeView f (uncurry (:==:))
  where
    f (lhs :==: rhs) = do
      v            <- selectVar (lhs .-. rhs)
      (lhs', rhs') <- varLConR v lhs rhs
      (c, (b, e))  <- match strictPowerView lhs'
      return (lhs, rhs ./. c)
