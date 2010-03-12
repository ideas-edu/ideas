{-# OPTIONS -XGeneralizedNewtypeDeriving #-}
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
module Domain.Math.Polynomial.Equivalence 
   ( linEq, quadrEqContext, simLogic
   ) where

import Common.Context
import Common.Traversable
import Common.View
import Data.Maybe
import Data.List (sort, nub)
import Domain.Math.Polynomial.Views
import Prelude hiding ((^), sqrt)
import Domain.Logic.Formula hiding (Var, disjunctions)
import qualified Domain.Logic.Formula as Logic
import Domain.Math.Numeric.Views
import Domain.Math.Data.Relation
import Domain.Math.Data.Interval
import Domain.Math.SquareRoot.Views
import Domain.Math.Expr
import Domain.Math.Data.SquareRoot
import Control.Monad
import Domain.Math.Clipboard
import Common.Rewriting hiding (match)
import Common.Uniplate

------------------------------------------------------------------
-- Steps for solving an inequation:
-- * solve as an equation
-- * determine intervals (based on solutions in previous steps)

{-
example :: Logic (Relation Expr)
example = (atom (x^2 .>=. 4) :||: atom (x .<. 1)) :&&: atom (x ./=. 0)
 where x = Var "x"
       atom = Logic.Var -}

relationIntervals :: Ord a => RelationType -> a -> Intervals a
relationIntervals relType a = 
   case relType of
      EqualTo              -> only singleton a
      NotEqualTo           -> except a
      LessThan             -> only lessThan a
      GreaterThan          -> only greaterThan a
      LessThanOrEqualTo    -> only lessThanOrEqualTo a
      GreaterThanOrEqualTo -> only greaterThanOrEqualTo a
      Approximately        -> only singleton a -- i.e., equalTo
 where    
   only f = fromList . return . f

logicIntervals :: Ord a => Logic (Intervals a) -> Intervals a
logicIntervals = foldLogic 
   ( id
   , \p q -> complement p `union` q -- p->q  =  ~p||q
   , \p q -> (p `intersect` q) `union` (complement p `intersect` complement q)  -- p<->q  =  (p&&q)||(~p&&~q)
   , intersect
   , union
   , complement
   , fromList [unbounded]
   , fromList [empty]
   )
 
 {-  
test :: Maybe (Logic (SquareRoot Rational))
test = fmap (catLogic . fmap orToLogic) $ switch $ fmap (fmap (fmap snd) . match quadraticEquationView . toEquation) example
 where
   toEquation r = leftHandSide r :==: rightHandSide r
   orToLogic :: OrList a -> Logic a
   orToLogic xs = case disjunctions xs of
                     Just ys -> if null ys then F else foldr1 (:||:) (map Logic.Var ys)
                     Nothing -> T
       
-- Precondition: argument list is sorted      
regions :: (Fractional a, Ord a) => [a] -> [(a, Interval a)]
regions []        = [(0, unbounded)]
regions as@(a:_)  = (a-1, lessThan a) : f as
 where
   f xs@(x:_)     = (x, singleton x) : g xs
   f []           = []
   g (x:xs@(y:_)) = ((x+y)/2, open x y) : f xs
   g [x]          = [(x+1, greaterThan x)]
   g _            = []
   
solvedView :: View (Equation Expr) (OrList (Equation Expr))
solvedView = makeView f g 
 where
   f eq = do
      as <- match higherDegreeEquationsView (return eq)
      let f a = return (a :==: 0)
      switch (fmap f as)
   g xs = case disjunctions xs of
             Just ds -> 
                let list = [ a-b | a :==: b <- ds ]
                in build productView (False, list) :==: 0 
             Nothing -> 
                0 :==: 0
   
q = match solvedView ex 
 where
   ex = x^2 - 3*x + 4 :==: 5 - x
   x  = Var "x" -}

-----------------------------------------------------------
             
linEq :: Relation Expr -> Relation Expr -> Bool
linEq a b = fromMaybe False $ liftM2 (==) (linRel a) (linRel b)

linRel :: Relation Expr -> Maybe (String, Intervals Rational)
linRel = linRelWith rationalView

linRelWith :: (Ord a, Fractional a)
           => View Expr a -> Relation Expr -> Maybe (String, Intervals a)
linRelWith v rel =
   case match (linearViewWith v) (lhs - rhs) of
      Nothing -> Nothing
      Just (s, a, b) 
         | a==0 -> 
              return (s, fromList [ unbounded | b==0 ])
         | otherwise -> do
              let zero = -b/a
                  tp = relationType $ (if a<0 then flipSides else id) rel
              return (s, relationIntervals tp zero) 
 where
   lhs = leftHandSide rel
   rhs = rightHandSide rel

newtype Q = Q (SquareRoot Rational) deriving (Show, Eq, Num, Fractional)

-- Use normal (numeric) ordering on square roots
instance Ord Q where
   Q a `compare` Q b = f a `compare` f b 
    where
      f :: SquareRoot Rational -> Double
      f = eval . fmap fromRational

qView :: View (SquareRoot Rational) Q
qView = makeView (return . Q) (\(Q a) -> a)

quadrEqContext :: Context (Logic (Relation Expr)) -> Context (Logic (Relation Expr)) -> Bool
quadrEqContext a b = isJust $ do
   let clipA = ineqOnClipboard a
   let clipB = ineqOnClipboard b
   termA <- fromContext a
   termB <- fromContext b
   -- test clipboard 
   when (isJust clipA || isJust clipB) $
      guard $ quadrEq (fromMaybe termA clipA) (fromMaybe termB clipB)
   -- test terms
   let f x = if isJust x then toEq else id
   guard $ quadrEq (f clipB termA) (f clipA termB)

toEq :: Logic (Relation Expr) -> Logic (Relation Expr)
toEq (Logic.Var rel) = Logic.Var (leftHandSide rel .==. rightHandSide rel)
toEq (p :&&: q) = toEq p :||: toEq q -- workaround for -3<x<5 
toEq (p :||: q) = toEq p :||: toEq q
toEq p = p

ineqOnClipboard :: Context a -> Maybe (Logic (Relation Expr))
ineqOnClipboard = evalCM $ const $ do
   expr <- lookupClipboard "ineq"
   fromExpr expr


quadrEq :: Logic (Relation Expr) -> Logic (Relation Expr) -> Bool
quadrEq p q = fromMaybe False $ do
   xs <- switch (fmap quadrRel p)
   ys <- switch (fmap quadrRel q)
   let vs = map fst (crush xs ++ crush ys)
   guard (null vs || all (==head vs) vs)
   let ix = logicIntervals (fmap snd xs)
       iy = logicIntervals (fmap snd ys)
   if ix == iy then return True else return False -- error $ show (p, q, ix, iy)

quadrRel :: Relation Expr -> Maybe (String, Intervals Q)
quadrRel rel = 
   case match (quadraticViewWith rationalView) (lhs - rhs) of
      Nothing ->
         linRelWith (squareRootViewWith rationalView >>> qView) rel
      Just (s, xa, xb, xc) -> do
         let (tp, a, b, c) 
                | xa<0 = 
                     (relationType (flipSides rel), -xa, -xb, -xc)
                | otherwise =
                     (relationType rel, xa, xb, xc)
             discr = b*b - 4*a*c
             pa = Q ((-fromRational b-sqrtRational discr) / (2 * fromRational a))
             pb = Q ((-fromRational b+sqrtRational discr) / (2 * fromRational a))
         guard (a/=0)
         (\is -> Just (s, is)) $
          case compare discr 0 of
            LT | tp `elem` [NotEqualTo, GreaterThan, GreaterThanOrEqualTo] ->
                    fromList [unbounded]
               | tp `elem` [EqualTo, Approximately, LessThan, LessThanOrEqualTo] ->
                    fromList [empty]
            EQ | tp `elem` [EqualTo, Approximately, LessThanOrEqualTo] -> 
                    fromList [singleton pa]
               | tp == NotEqualTo ->
                    except pa
               | tp == LessThan ->
                    fromList [empty]
               | tp == GreaterThan ->
                    except pa
               | tp == GreaterThanOrEqualTo ->
                    fromList [unbounded]
            GT | tp `elem` [EqualTo, Approximately] -> 
                    fromList [singleton pa, singleton pb]
               | tp == NotEqualTo -> 
                    except pa `intersect` except pb
               | tp == LessThan -> 
                    fromList [open pa pb]
               | tp == LessThanOrEqualTo ->
                    fromList [closed pa pb]
               | tp == GreaterThan -> 
                    fromList [lessThan pa, greaterThan pb]
               | tp == GreaterThanOrEqualTo ->
                    fromList [lessThanOrEqualTo pa, greaterThanOrEqualTo pb]
            _ -> error "unknown case in quadrRel"
 where
   lhs = leftHandSide rel
   rhs = rightHandSide rel
   
   
-- for similarity 
simLogic :: Ord a => (a -> a) -> Logic a -> Logic a -> Bool
simLogic f a b 
   | isOperator orOperator a =
        let collect = nub . sort . trueOr . collectOr
        in eqList (simLogic f) (collect a) (collect b)
   | isOperator andOperator a =
        let collect = nub . sort . falseAnd . collectAnd
        in eqList (simLogic f) (collect a) (collect b)
   | otherwise = 
        shallowEq a b && eqList (simLogic f) (children a) (children b)
 where
   eqList eq xs ys = 
      length xs == length ys && and (zipWith eq xs ys) 
 
   collectOr (p :||: q) = collectOr p ++ collectOr q
   collectOr F = []
   collectOr a = [a]
   
   trueOr xs = if T `elem` xs then [] else xs
   
   collectAnd (p :&&: q) = collectAnd p ++ collectAnd q
   collectAnd T = []
   collectAnd a = [a]
   
   falseAnd xs = if F `elem` xs then [] else xs