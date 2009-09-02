module Domain.Math.Polynomial.Views
   ( polyView, polyViewFor, polyViewWith, polyViewForWith
   , quadraticView, quadraticViewFor, quadraticViewWith, quadraticViewForWith
   , linearView, linearViewFor, linearViewWith, linearViewForWith
   , constantPolyView, linearPolyView, quadraticPolyView, cubicPolyView
   , monomialPolyView, binomialPolyView, trinomialPolyView
   , polyNormalForm
   , linearEquationView, quadraticEquationView, quadraticEquationsView, eqHD
   ) where

import Prelude hiding ((^))
import Control.Monad
import Data.List
import Common.Utils (distinct)
import Domain.Math.Data.Polynomial
import Domain.Math.Data.Equation
import Domain.Math.Data.OrList
import Domain.Math.Expr
import Data.Maybe
import qualified Domain.Math.Data.SquareRoot as SQ
import Domain.Math.Expr.Symbols
import Domain.Math.SquareRoot.Views
import Domain.Math.View.Basic
import Common.Traversable
import Domain.Math.View.Power (powerFactorViewForWith)

-------------------------------------------------------------------
-- Polynomial view

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
           liftM2 power (f a) (match integralView n) -- non-negative??
         _ -> do 
            guard (pv `notElem` collectVars expr)
            liftM con (match v expr)
   
   g        = build sumView . map h . reverse . terms
   h (a, n) = build v a .*. (Var pv .^. fromIntegral n)

-------------------------------------------------------------------
-- Quadratic view

quadraticView :: View Expr (String, Expr, Expr, Expr)
quadraticView = quadraticViewWith identity

quadraticViewFor :: String -> View Expr (Expr, Expr, Expr)
quadraticViewFor v = quadraticViewForWith v identity

quadraticViewWith :: Fractional a => View Expr a -> View Expr (String, a, a, a)
quadraticViewWith v = polyViewWith v >>> second quadraticPolyView >>> makeView f g
 where
   f (s, (a, b, c)) = return (s, a, b, c)
   g (s, a, b, c)   = (s, (a, b, c))

quadraticViewForWith :: Fractional a => String -> View Expr a -> View Expr (a, a, a)
quadraticViewForWith pv v = polyViewForWith pv v >>> quadraticPolyView

-------------------------------------------------------------------
-- Linear view

linearView :: View Expr (String, Expr, Expr)
linearView = linearViewWith identity

linearViewFor :: String -> View Expr (Expr, Expr)
linearViewFor v = linearViewForWith v identity

linearViewWith :: Fractional a => View Expr a -> View Expr (String, a, a)
linearViewWith v = polyViewWith v >>> second linearPolyView >>> makeView f g
 where
   f (s, (a, b)) = return (s, a, b)
   g (s, a, b)   = (s, (a, b))

linearViewForWith :: Fractional a => String -> View Expr a -> View Expr (a, a)
linearViewForWith pv v = polyViewForWith pv v >>> linearPolyView

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

isList1 [a]          = Just (a)
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
{-
orListView :: ([b] -> Maybe c) -> (c -> [b]) -> View a b -> View (OrList a) c
orListView z z2 v = makeView f g
 where
   f (OrList xs) = join $ liftM z (mapM (match v) xs)
   g = OrList . map (build v) . z2 -}
   
linearEquationView :: View (Equation Expr) (String, Rational)
linearEquationView = makeView f g
 where
   f (lhs :==: rhs) = do 
      (x, a, b) <- match (linearViewWith rationalView) (lhs - rhs)
      return (x, -b/a)
   g (x, r) = Var x :==: fromRational r

quadraticEquationsView:: View (OrList (Equation Expr)) (String, [SQ.SquareRoot Rational])
quadraticEquationsView = makeView f g
 where
   f (OrList xs) = do 
      ps <- mapM (match quadraticEquationView) xs
      case unzip ps of
         ([], _)     -> 
            return ([], [])
         (s:ss, xss) -> do
            guard (all (==s) ss)
            let make = sort . nub . filter (not . SQ.imaginary) . concat
            return (s, make xss)
            
   g (s, xs) = OrList [ Var s :==: build squareRootView rhs | rhs <- xs ]

quadraticEquationView:: View (Equation Expr) (String, [SQ.SquareRoot Rational])
quadraticEquationView = makeView f g
 where
   f (lhs :==: rhs) = do
      (s, poly) <- match polyView (lhs - rhs)
      liftM ((,) s) $
         case degree poly < 2 of
            True -> do
               polySQ <- switch $ fmap (match (squareRootViewWith rationalView)) poly
               (a, b) <- match linearPolyView polySQ
               if a==0 then (if b==0 then Just [] else Nothing)
                       else return [-(b/a)]
            False -> do
               polyR <- switch $ fmap (match rationalView) poly
               (a, b, c) <- match quadraticPolyView polyR
               if a == 0 then return [SQ.con (-(c/b))] else do -- BAH!
               let discr  = b*b - 4*a*c
                   sdiscr = SQ.sqrtRational discr
               case compare discr 0 of
                  LT -> Just []
                  EQ -> Just [ SQ.con (-b/(2*a)) ]
                  GT -> Just [ SQ.scale (1/(2*a)) (-SQ.con b + sdiscr)
                             , SQ.scale (1/(2*a)) (-SQ.con b - sdiscr)
                             ]

   g (s, as) = build productView (False, map (h s) as) :==: 0
   h s a = simplify sumView (Var s - build squareRootView a)
   
eqHD :: OrList (Equation Expr) -> OrList (Equation Expr) -> Bool
eqHD a b = f a == f b
 where
   f (OrList xs) = sort $ nub $ concatMap normHD xs

normHD :: Equation Expr -> [Expr]
normHD (x :==: y) = 
   case toPoly (x-y) of
      Just p  -> concatMap g $ factorize p
      Nothing -> 
         case (x, y) of 
            (Var _, e) | noVars e -> [simplify squareRootView e]
            _ -> error $ show (x,y)
 where
   g :: Polynomial Rational -> [Expr]
   g p | d==0 = []
       | length (terms p) <= 1 = [0]
       | d==1 = [fromRational $ coefficient 0 p / coefficient 1 p]
       | d==2 = let [a,b,c] = [ coefficient n p | n <- [2,1,0] ]
                    discr   = b*b - 4*a*c
                in if discr < 0 then [] else 
                   map (simplify squareRootView)
                   [ (-fromRational b + sqrt (fromRational discr)) / 2 * fromRational a 
                   , (-fromRational b - sqrt (fromRational discr)) / 2 * fromRational a
                   ]
       | otherwise     = error ("NOT SOLVED:" ++ show p) -- fromPoly
    where d = degree p

toPoly :: Expr -> Maybe (Polynomial Rational)
toPoly e = do
   (_, p) <- match polyView e
   switch (fmap (match rationalView) p)