-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
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
   ( polyView, polyViewWith
   , quadraticView, quadraticViewWith --, quadraticViewFor quadraticViewForWith
   , linearView, linearViewWith -- linearViewFor linearViewForWith
   , constantPolyView, linearPolyView, quadraticPolyView, cubicPolyView
   , monomialPolyView, binomialPolyView, trinomialPolyView
   , polyNormalForm, polyRelaxedForm
   , linearEquationView, quadraticEquationView, quadraticEquationsView
   , higherDegreeEquationsView, listOfPowerFactors
   ) where

import Common.Classes
import Common.Rewriting
import Common.Utils (distinct)
import Common.Utils.Uniplate (transform, descend, children)
import Common.View
import Control.Monad
import Data.Foldable (foldMap, toList)
import Data.Maybe
import Data.Traversable (mapM)
import Domain.Math.CleanUp
import Domain.Math.Data.OrList
import Domain.Math.Data.Polynomial
import Domain.Math.Data.Relation
import Domain.Math.Equation.CoverUpRules
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import Domain.Math.Power.OldViews (powerFactorViewForWith)
import Domain.Math.SquareRoot.Views
import Prelude hiding ((^))
import qualified Domain.Math.Data.SquareRoot as SQ
import qualified Prelude

polyViewWithNew :: View (String, Expr) (String, Polynomial Expr)
polyViewWithNew = makeView matchPoly buildPoly
 where
   matchPoly (s, expr) = liftM ((,) s) (matchPolyFor s expr)
   buildPoly (s, p)    = (s, buildPolyFor s p)

   matchPolyFor pv expr =
      case expr of
         Var s | pv == s -> Just var
         Nat n    -> Just (fromIntegral n)
         Negate a -> liftM negate (f a)
         a :+: b  -> liftM2 (+) (f a) (f b)
         a :-: b  -> liftM2 (-) (f a) (f b)
         a :*: b  -> liftM2 (*) (f a) (f b)
         a :/: b  -> do
            guard (withoutVar pv b)
            p <- f a
            d <- match rationalApproxView b
            guard (d /= 0)
            return (fmap (/fromRational d) p)
         Sym s [a, n] | isPowerSymbol s ->
           liftM2 (Prelude.^) (f a) (matchNat n)
         _ -> do
            guard (withoutVar pv expr)
            return (con expr)
    where
      f = matchPolyFor pv

   buildPolyFor pv =
      let f (a, n) = a .*. (Var pv .^. fromIntegral n)
      in build sumView . map f . reverse . terms

   matchNat expr = do
      n <- match integerView expr
      guard (n >= 0)
      return n

-------------------------------------------------------------------
-- Polynomial view

polyView :: View Expr (String, Polynomial Expr)
polyView = (f <-> snd) >>> polyViewWithNew
 where
   f a = (fromMaybe "" (selectVar a), a)

polyViewWith :: Fractional a => View Expr a -> View Expr (String, Polynomial a)
polyViewWith v = polyView >>> second (traverseView v)

-------------------------------------------------------------------
-- Quadratic view

quadraticView :: View Expr (String, Expr, Expr, Expr)
quadraticView = quadraticViewWith identity

quadraticViewWith :: Fractional a => View Expr a -> View Expr (String, a, a, a)
quadraticViewWith v = polyViewWith v >>> second quadraticPolyView >>> (f <-> g)
 where
   f (s, (a, b, c)) = (s, a, b, c)
   g (s, a, b, c)   = (s, (a, b, c))

-------------------------------------------------------------------
-- Linear view

linearView :: View Expr (String, Expr, Expr)
linearView = linearViewWith identity

linearViewWith :: Fractional a => View Expr a -> View Expr (String, a, a)
linearViewWith v = polyViewWith v >>> second linearPolyView >>> (f <-> g)
 where
   f (s, (a, b)) = (s, a, b)
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
   | otherwise = sum (map f as)
 where
   f (a, n) = con a * var Prelude.^ n

polynomialList :: Num a => Polynomial a -> [a]
polynomialList p = map (`coefficient` p) [d, d-1 .. 0]
 where d = degree p

list1 :: a -> [a]
list1 a = [a]

list2 :: (a, a) -> [a]
list2 (a, b)     = [a, b]

list3 :: (a, a, a) -> [a]
list3 (a, b, c) = [a, b, c]

list4 :: (a, a, a, a) -> [a]
list4 (a, b, c, d) = [a, b, c, d]

isList1 :: [a] -> Maybe a
isList1 [a] = Just a
isList1 _   = Nothing

isList2 :: [a] -> Maybe (a, a)
isList2 [a, b] = Just (a, b)
isList2 _      = Nothing

isList3 :: [a] -> Maybe (a, a, a)
isList3 [a, b, c] = Just (a, b, c)
isList3 _         = Nothing

isList4 :: [a] -> Maybe (a, a, a, a)
isList4 [a, b, c, d] = Just (a, b, c, d)
isList4 _            = Nothing

-------------------------------------------------------------------
-- Normal form, and list of power factors

listOfPowerFactors :: Num a => String -> View Expr a -> View Expr [(a, Int)]
listOfPowerFactors pv v =
   toView sumView >>> listView (powerFactorViewForWith pv v)

-- Generalization
polyForm :: Num a => Bool -> View Expr a -> View Expr (String, Polynomial a)
polyForm relaxed v = makeView f (uncurry g)
 where
   f e = do
      pv <- selectVar e
      xs <- match (listOfPowerFactors pv v) e
      guard (relaxed || distinct (map snd xs))
      return (pv, buildPairs xs)
   g pv = build (listOfPowerFactors pv v) . reverse . terms

polyNormalForm :: Num a => View Expr a -> View Expr (String, Polynomial a)
polyNormalForm = polyForm False

-- relaxes the condition that all powers should be distinct
polyRelaxedForm :: Num a => View Expr a -> View Expr (String, Polynomial a)
polyRelaxedForm = polyForm True

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
linearEquationView = linearEquationViewWith rationalApproxView

quadraticEquationsView:: View (OrList (Equation Expr)) (OrList (String, SQ.SquareRoot Rational))
quadraticEquationsView = makeView f (fmap g)
 where
   f = liftM (simplify orSetView . foldMap id)
          . Data.Traversable.mapM (match quadraticEquationView)

   g (x, a) = Var x :==: build (squareRootViewWith rationalApproxView) a

quadraticEquationView :: View (Equation Expr) (OrList (String, SQ.SquareRoot Rational))
quadraticEquationView = makeView f g
 where
   f (lhs :==: rhs) = do
      (s, p) <- match (polyViewWith (squareRootViewWith rationalApproxView)) (lhs - rhs)
      guard (degree p <= 2)
      liftM (fmap ((,) s)) $
         case polynomialList p of
            [a, b, c] -> do
               discr <- SQ.fromSquareRoot (b*b - SQ.scale 4 (a*c))
               let sdiscr = SQ.sqrtRational discr
                   twoA   = SQ.scale 2 a
               case compare discr 0 of
                  LT   -> return false
                  EQ   -> return $ singleton (-b/twoA)
                  GT   -> return $ toOrList [(-b+sdiscr)/twoA, (-b-sdiscr)/twoA]
            [a, b]     -> return $ singleton (-b/a)
            [a] | a==0 -> return true
            _          -> return false

   g xs | isTrue xs = 0 :==: 0
        | otherwise = build productView (False, map make (toList xs)) :==: 0
    where
      make (x, a) = Var x .-. build (squareRootViewWith rationalApproxView) a

higherDegreeEquationsView :: View (OrList (Equation Expr)) (OrList Expr)
higherDegreeEquationsView = f <-> fmap (:==: 0)
 where
   f    = simplify orSetView . foldMap make . coverUpOrs
   make = toOrList . filter (not . hasNegSqrt)
        . map (cleanUpExpr . distr) . normHDE . sub
   sub (a :==: b) = a-b

   distr = transform g
    where
      g ((a :+: b) :/: c) = (a ./. c) .+. (b ./. c)
      g ((a :-: b) :/: c) = (a ./. c) .-. (b ./. c)
      g a = a

hasNegSqrt :: Expr -> Bool
hasNegSqrt (Sqrt a) =
   case match rationalApproxView a of
      Just r | r < 0 -> True
      _ -> hasNegSqrt a
hasNegSqrt (Sym s [a, b]) | isRootSymbol s =
   case (match rationalApproxView a, match integerView b) of
      (Just r, Just n) | r < 0 && even n -> True
      _ -> hasNegSqrt a || hasNegSqrt b
hasNegSqrt a =
   any hasNegSqrt (children a)

normHDE :: Expr -> [Expr]
normHDE e =
   case match (polyViewWith rationalApproxView) e of
      Just (x, p)  -> normPolynomial x p
      Nothing -> fromMaybe [e] $ do
         (x, a) <- match (linearEquationViewWith (squareRootViewWith rationalApproxView)) (e :==: 0)
         return [ Var x .+. build (squareRootViewWith rationalApproxView) (-a) ]

normPolynomial :: String -> Polynomial Rational -> [Expr]
normPolynomial x p
   | degree p == 0 =
        []
   | length (terms p) <= 1 =
        [Var x]
   | degree p == 1 =
        [Var x .+. fromRational (coefficient 0 p / coefficient 1 p)]
   | degree p == 2 =
        let [a,b,c] = [ coefficient n p | n <- [2,1,0] ]
            discr   = b*b - 4*a*c
            sdiscr  = SQ.sqrtRational discr
        in if discr < 0 then [] else
           map ((Var x .+.) . build (squareRootViewWith rationalApproxView))
           [ SQ.scale (1/(2*a)) (SQ.con b + sdiscr)
           , SQ.scale (1/(2*a)) (SQ.con b - sdiscr)
           ]
   | otherwise =
        case terms p of
           [(c, 0), (b, e1), (a, e2)] | e1 > 1 && e2 `mod` e1 == 0 ->
              let list = [(c, 0), (b, 1), (a, e2 `div` e1)]
                  newp = sum (map (\(y, z) -> con y * (var Prelude.^ z)) list)
                  sub  = map (substitute (x, Var x^fromIntegral e1))
              in concatMap normHDE (sub (normPolynomial x newp))
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
                 ps | length ps > 1 -> concatMap (normPolynomial x) ps
                 _ -> [build (polyViewWith rationalApproxView) (x, p)]

substitute :: (String, Expr) -> Expr -> Expr
substitute (s, a) (Var b) | s==b = a
substitute pair expr = descend (substitute pair) expr