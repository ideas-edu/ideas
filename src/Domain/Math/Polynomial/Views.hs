module Domain.Math.Polynomial.Views
   ( polyView, polyViewFor, polyViewWith, polyViewForWith
   , quadraticView, quadraticViewFor, quadraticViewWith, quadraticViewForWith
   , linearView, linearViewFor, linearViewWith, linearViewForWith
   , constantPolyView, linearPolyView, quadraticPolyView, cubicPolyView
   , monomialPolyView, binomialPolyView, trinomialPolyView
   , polyNormalForm
   ) where

import Prelude hiding ((^))
import Control.Monad
import Data.List
import Common.Utils (distinct)
import Domain.Math.Data.Polynomial
import Domain.Math.Expr
import Domain.Math.Expr.Symbols
import Domain.Math.Numeric.Generators (rationalGenerator)
import Domain.Math.View.Basic hiding (linearView)
import Domain.Math.View.Power (powerFactorViewForWith)
import Test.QuickCheck

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
-- Generators

-- tailored towards generating "int" expressions (also prevents 
-- division by zero)
polyGenerator :: String -> Maybe Int -> Int -> Gen Expr
polyGenerator v degree = symbolGenerator extras syms
 where
   syms = [plusSymbol, timesSymbol, minusSymbol, negateSymbol]
   extras n = natGenerator:varGenerator [v]:[ g | n > 0, g <- [divGen n, powerGen n] ]
   divGen n = do
      e <- polyGenerator v degree (n `div` 2)
      r <- rationalGeneratorNZ (n `div` 2)
      return (e :/: r)
   powerGen n = do
      e <- polyGenerator v degree (n `div` 2)
      n <- choose (0, 10)
      return (e ^ fromInteger n)

rationalGeneratorNZ :: Int -> Gen Expr -- non-zero
rationalGeneratorNZ n = do
   expr <- rationalGenerator n
   case match rationalView expr of
      Just r | r /= 0 -> return expr
      _               -> rationalGeneratorNZ n
   

testPolyGen = quickCheck $ forAll (sized (polyGenerator "x" Nothing)) (`belongsTo` polyViewWith rationalView)

t1 = replicateM_ 10 testPolyGen
t2 = verboseCheck $ forAll (sized (polyGenerator "x" Nothing)) $ \_ -> True