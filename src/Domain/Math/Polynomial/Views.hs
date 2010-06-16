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
module Domain.Math.Polynomial.Views
   ( polyView, polyViewWith -- polyViewFor, polyViewForWith
   , quadraticView, quadraticViewWith --, quadraticViewFor quadraticViewForWith
   , linearView, linearViewWith -- linearViewFor linearViewForWith
   , constantPolyView, linearPolyView, quadraticPolyView, cubicPolyView
   , monomialPolyView, binomialPolyView, trinomialPolyView
   , polyNormalForm
   , linearEquationView, quadraticEquationView, quadraticEquationsView
   , higherDegreeEquationsView
   ) where

import Prelude hiding ((^))
import Control.Monad
import Common.Apply
import Common.Context
import Common.View
import Common.Traversable
import Common.Uniplate (transform, uniplate, children)
import Common.Utils (distinct)
import Domain.Math.Data.Polynomial
import Domain.Math.Data.Relation
import Domain.Math.Data.OrList
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import Domain.Math.Equation.CoverUpExercise
import Domain.Math.Polynomial.CleanUp
import Data.Maybe
import qualified Domain.Math.Data.SquareRoot as SQ
import Domain.Math.SquareRoot.Views
import Domain.Math.Power.Views (powerFactorViewForWith)

-------------------------------------------------------------------
-- Polynomial view

polyView :: View Expr (String, Polynomial Expr)
polyView = polyViewWith identity

polyViewWith :: Fractional a => View Expr a -> View Expr (String, Polynomial a)
polyViewWith v = makeView matchPoly (uncurry buildPoly)
 where
   matchPoly expr = do 
      pv <- selectVar expr
      p  <- matchPolyFor pv expr
      return (pv, p) 

   matchPolyFor pv expr =
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
           liftM2 power (f a) (matchNat n)
         _ -> do 
            guard (pv `notElem` collectVars expr)
            liftM con (match v expr)
    where
      f = matchPolyFor pv
   
   buildPoly pv = 
      let f (a, n) = build v a .*. (Var pv .^. fromIntegral n)
      in build sumView . map f . reverse . terms
   
   
   matchNat expr = do
      n <- match integralView expr
      guard (n >= 0)
      return n

-------------------------------------------------------------------
-- Quadratic view

quadraticView :: View Expr (String, Expr, Expr, Expr)
quadraticView = quadraticViewWith identity

quadraticViewWith :: Fractional a => View Expr a -> View Expr (String, a, a, a)
quadraticViewWith v = polyViewWith v >>> second quadraticPolyView >>> makeView f g
 where
   f (s, (a, b, c)) = return (s, a, b, c)
   g (s, a, b, c)   = (s, (a, b, c))

-------------------------------------------------------------------
-- Linear view

linearView :: View Expr (String, Expr, Expr)
linearView = linearViewWith identity

linearViewWith :: Fractional a => View Expr a -> View Expr (String, a, a)
linearViewWith v = polyViewWith v >>> second linearPolyView >>> makeView f g
 where
   f (s, (a, b)) = return (s, a, b)
   g (s, a, b)   = (s, (a, b))

-------------------------------------------------------------------
-- Views on polynomials (degree)

constantPolyView :: Num a => View (Polynomial a) a
constantPolyView = makeView (isList1 . polynomialList) (buildList . list1)
    
linearPolyView :: Num a => View (Polynomial a) (a, a)
linearPolyView = makeView (isList2 . polynomialList) (buildList . list2)
 
quadraticPolyView :: Num a => View (Polynomial a) (a, a, a)
quadraticPolyView = makeView (isList3 . polynomialList) (buildList . list3)
    
cubicPolyView :: Num a => View (Polynomial a) (a, a, a, a)
cubicPolyView = makeView (isList4 . polynomialList) (buildList . list4)

-------------------------------------------------------------------
-- Views on polynomials (number of terms)

monomialPolyView :: Num a => View (Polynomial a) (a, Int)
monomialPolyView = makeView (isList1. terms) (buildPairs . list1)

binomialPolyView :: Num a => View (Polynomial a) ((a, Int), (a, Int))
binomialPolyView = makeView (isList2 . terms) (buildPairs . list2)

trinomialPolyView :: Num a => View (Polynomial a) ((a, Int), (a, Int), (a, Int))
trinomialPolyView = makeView (isList3 . terms) (buildPairs . list3)

-- helpers
buildList :: Num a => [a] -> Polynomial a
buildList = buildPairs . flip zip [0..] . reverse

buildPairs :: Num a => [(a, Int)] -> Polynomial a
buildPairs as 
   | null as   = 0
   | otherwise = foldl1 (+) (map f as)
 where
   f (a, n) = con a * power var n
 
polynomialList :: Num a => Polynomial a -> [a]
polynomialList p = map (`coefficient` p) [d, d-1 .. 0]
 where d = degree p

list1 (a)          = [a]
list2 (a, b)       = [a, b]
list3 (a, b, c)    = [a, b, c]
list4 (a, b, c, d) = [a, b, c, d]

isList1 [a]          = Just a
isList1 _            = Nothing
isList2 [a, b]       = Just (a, b)
isList2 _            = Nothing
isList3 [a, b, c]    = Just (a, b, c)
isList3 _            = Nothing
isList4 [a, b, c, d] = Just (a, b, c, d)
isList4 _            = Nothing

-------------------------------------------------------------------
-- Normal form, and list of power factors

listOfPowerFactors :: Num a => String -> View Expr a -> View Expr [(a, Int)]
listOfPowerFactors pv v = sumView >>> listView (powerFactorViewForWith pv v)

polyNormalForm :: Num a => View Expr a -> View Expr (String, Polynomial a)
polyNormalForm v = makeView f (uncurry g)
 where
   f e = do
      pv <- selectVar e
      xs <- match (listOfPowerFactors pv v) e
      guard (distinct (map snd xs))
      return (pv, buildPairs xs)
   g pv = build (listOfPowerFactors pv v) . reverse . terms
   
-------------------------------------------------------------------
-- Normal forms for equations

-- Excludes equations such as 1==1 or 0==1
linearEquationViewWith :: Fractional a => View Expr a -> View (Equation Expr) (String, a)
linearEquationViewWith v = makeView f g
 where
   f (lhs :==: rhs) = do 
      (x, a, b) <- match (linearViewWith v) (lhs - rhs)
      return (x, -b/a)
   g (x, r) = Var x :==: build v r
   
linearEquationView :: View (Equation Expr) (String, Rational)
linearEquationView = linearEquationViewWith rationalView

quadraticEquationsView:: View (OrList (Equation Expr)) (OrList (String, SQ.SquareRoot Rational))
quadraticEquationsView = makeView f (fmap g)
 where
   f eq = do 
      ors <- switch (fmap (match quadraticEquationView) eq)
      return (normalize (join ors))

   g (x, a) = Var x :==: build (squareRootViewWith rationalView) a

quadraticEquationView :: View (Equation Expr) (OrList (String, SQ.SquareRoot Rational))
quadraticEquationView = makeView f g
 where
   f (lhs :==: rhs) = do
      (s, p) <- match (polyViewWith (squareRootViewWith rationalView)) (lhs - rhs)
      guard (degree p <= 2)
      liftM (fmap ((,) s)) $
         case polynomialList p of
            [a, b, c] -> do
               discr <- SQ.fromSquareRoot (b*b - SQ.scale 4 (a*c))
               let sdiscr = SQ.sqrtRational discr
                   twoA   = SQ.scale 2 a
               case compare discr 0 of
                  LT   -> return false
                  EQ   -> return $ orList [-b/twoA]
                  GT   -> return $ orList [(-b+sdiscr)/twoA, (-b-sdiscr)/twoA]
            [a, b]     -> return $ orList [-b/a]
            [a] | a==0 -> return true
            _          -> return false
   
   g ors = 
      case disjunctions ors of
         Nothing -> 0 :==: 0
         Just xs -> 
            let make (x, a) = Var x .-. build (squareRootViewWith rationalView) a
            in build productView (False, map make xs) :==: 0

higherDegreeEquationsView :: View (OrList (Equation Expr)) (OrList Expr)
higherDegreeEquationsView = makeView f (fmap g)
 where
   f = let make (a :==: b) = orList (filter (not . hasNegSqrt) $ map cleanUpExpr2 (normHDE (a-b)))
       in Just . normalize . join . fmap make . cuRules

   g = (:==: 0)
   
   cuRules :: OrList (Equation Expr) -> OrList (Equation Expr)
   cuRules xs = 
      let new  = fmap (fmap (cleanUpExpr2 . distr)) xs
          newc = newContext emptyEnv (exprNavigator new)
      in case apply coverUpStrategy newc >>= fromContext of
            Just ys | xs /= ys -> cuRules ys
            _ -> new

hasNegSqrt :: Expr -> Bool
hasNegSqrt (Sqrt a) = 
   case match rationalView a of
      Just r | r < 0 -> True
      _ -> hasNegSqrt a
hasNegSqrt (Sym s [a, b]) | s == rootSymbol = 
   case (match rationalView a, match integerView b) of
      (Just r, Just n) | r < 0 && even n -> True
      _ -> hasNegSqrt a || hasNegSqrt b
hasNegSqrt a = 
   any hasNegSqrt (children a)

distr :: Expr -> Expr
distr = transform f
 where
   f ((a :+: b) :/: c) = (a ./. c) .+. (b ./. c)
   f ((a :-: b) :/: c) = (a ./. c) .-. (b ./. c)
   f a = a

normHDE :: Expr -> [Expr]
normHDE e =
   case match (polyViewWith rationalView) e of
      Just (x, p)  -> g x p
      Nothing -> fromMaybe [e] $ do
         (x, a) <- match (linearEquationViewWith (squareRootViewWith rationalView)) (e :==: 0)
         return [ Var x .+. build (squareRootViewWith rationalView) (-a) ] 
 where
   g :: String -> Polynomial Rational -> [Expr]
   g x p 
       | d==0 = []
       | length (terms p) <= 1 = [Var x]
       | d==1 = [Var x .+. fromRational (coefficient 0 p / coefficient 1 p)]
       | d==2 = let [a,b,c] = [ coefficient n p | n <- [2,1,0] ]
                    discr   = b*b - 4*a*c
                    sdiscr  = SQ.sqrtRational discr
                in if discr < 0 then [] else 
                   map ((Var x .+.) . build (squareRootViewWith rationalView))
                   [ SQ.scale (1/(2*a)) (SQ.con b + sdiscr)
                   , SQ.scale (1/(2*a)) (SQ.con b - sdiscr)
                   ]
       | otherwise = 
            case terms p of 
               [(c, 0), (b, e1), (a, e2)] | e1 > 1 && e2 `mod` e1 == 0 -> 
                  let list = [(c, 0), (b, 1), (a, e2 `div` e1)]
                      newp = sum (map (\(x, y) -> scale x (power var y)) list)
                      sub  = map (substitute (x, Var x^fromIntegral e1))
                  in concatMap normHDE (sub (g x newp))
               [(c, 0), (a, n)]
                  | odd n  -> if c/a >= 0 
                              then [Var x + root (fromRational (c/a)) (fromIntegral n)]
                              else [Var x - root (fromRational (abs (c/a))) (fromIntegral n)]
                  | even n -> if c/a > 0
                              then []
                              else [ Var x + root (fromRational (abs (c/a))) (fromIntegral n) 
                                   , Var x - root (fromRational (abs (c/a))) (fromIntegral n)
                                   ]
               _ -> 
                  case factorize p of
                     ps | length ps > 1 -> concatMap (g x) ps
                     _ -> [build (polyViewWith rationalView) (x, p)]
    where 
      d = degree p
      
substitute :: (String, Expr) -> Expr -> Expr
substitute (s, a) (Var b) | s==b = a
substitute pair expr = f (map (substitute pair) cs)
 where 
   (cs, f) = uniplate expr