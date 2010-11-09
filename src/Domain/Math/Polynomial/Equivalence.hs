{-# OPTIONS -XGeneralizedNewtypeDeriving #-}
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
module Domain.Math.Polynomial.Equivalence 
   ( linEq, quadrEqContext, highEqContext, simLogic, flipGT
   , eqAfterSubstitution
   ) where

import Common.Classes
import Common.Context
import Common.Rewriting
import Common.Uniplate
import Common.View
import Control.Monad
import Data.List (sort, nub)
import Data.Maybe
import Domain.Logic.Formula hiding (Var, disjunctions)
import Domain.Math.Clipboard
import Domain.Math.Data.Interval
import Domain.Math.Data.Polynomial hiding (eval)
import Domain.Math.Data.Relation hiding (eval)
import Domain.Math.Data.SquareRoot
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import Domain.Math.Polynomial.CleanUp
import Domain.Math.Polynomial.Views
import Domain.Math.SquareRoot.Views
import Prelude hiding ((^), sqrt)
import qualified Domain.Logic.Formula as Logic

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

-----------------------------------------------------------
             
linEq :: Relation Expr -> Relation Expr -> Bool
linEq a b = fromMaybe False $ liftM2 (==) (linRel a) (linRel b)

linRel :: Relation Expr -> Maybe (String, Intervals Rational)
linRel = linRelWith rationalView

linRelWith :: (Ord a, Fractional a)
           => View Expr a -> Relation Expr -> Maybe (String, Intervals a)
linRelWith v rel =
   case match (linearViewWith v) (lhs - rhs) of
      Nothing -> 
         case match (polyViewWith v) (lhs - rhs) of
            Just (s, p) | degree p == 0 -> 
               case compare (coefficient 0 p) 0 of
                  LT | relationType rel `elem` [LessThan, LessThanOrEqualTo] -> 
                          return (s, fromList [unbounded])
                     | otherwise ->
                          return (s, fromList [empty])
                  EQ | relationType rel `elem` [EqualTo, LessThanOrEqualTo, GreaterThanOrEqualTo] -> 
                          return (s, fromList [unbounded])
                     | otherwise -> 
                          return (s, fromList [empty])
                  GT | relationType rel `elem` [GreaterThan, GreaterThanOrEqualTo] -> 
                          return (s, fromList [unbounded])
                     | otherwise ->
                          return (s, fromList [empty])
            _ -> Nothing
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
quadrEqContext = eqContextWith (polyEq quadrRel)

highEqContext :: Context (Logic (Relation Expr)) -> Context (Logic (Relation Expr)) -> Bool
highEqContext = eqContextWith (polyEq highRel)

eqContextWith eq a b = isJust $ do
   termA <- fromContext a
   termB <- fromContext b
   guard $ 
      case (ineqOnClipboard a, ineqOnClipboard b) of 
         (Just a,  Just b)  -> eq a b && eq termA termB
         (Just a,  Nothing) -> eq (fmap toEq a) termA && eq a termB
         (Nothing, Just b)  -> eq (fmap toEq b) termB && eq termA b
         (Nothing, Nothing) -> eq termA termB
 where
   toEq :: Relation Expr -> Relation Expr
   toEq r = leftHandSide r .==. rightHandSide r

ineqOnClipboard :: Context a -> Maybe (Logic (Relation Expr))
ineqOnClipboard = evalCM $ const $ do
   expr <- lookupClipboard "ineq"
   fromExpr expr

polyEq :: (Relation Expr -> Maybe (String, Intervals Q)) -> Logic (Relation Expr) -> Logic (Relation Expr) -> Bool
polyEq f p q = fromMaybe False $ do
   xs <- switch (fmap f p)
   ys <- switch (fmap f q)
   let vs = map fst (crush xs ++ crush ys)
   guard (null vs || all (==head vs) vs)
   let ix = logicIntervals (fmap snd xs)
       iy = logicIntervals (fmap snd ys)
   if ix == iy then return True else return False

cuPlus :: Relation Expr -> Maybe (Relation Expr)
cuPlus rel = do
   (a, b) <- match plusView (leftHandSide rel)
   guard (hasNoVar b && hasNoVar (rightHandSide rel))
   return $ constructor rel a (rightHandSide rel - b)
 `mplus` do
   (a, b) <- match plusView (leftHandSide rel)
   guard (hasNoVar a && hasNoVar (rightHandSide rel))
   return $ constructor rel b (rightHandSide rel - a)
 `mplus` do
   a <- isNegate (leftHandSide rel)
   return $ constructor (flipSides rel) a (-rightHandSide rel)

cuTimes :: Relation Expr -> Maybe (Relation Expr)
cuTimes rel = do
   (a, b) <- match timesView (leftHandSide rel)
   r1 <- match rationalView a
   r2 <- match rationalView (rightHandSide rel)
   guard (r1 /= 0)
   let make = if r1>0 then constructor rel else constructor (flipSides rel)
       new   = make b (build rationalView (r2/r1))
   return new

cuPower :: Relation Expr -> Maybe (Logic (Relation Expr))
cuPower rel = do
   (a, b) <- isBinary powerSymbol (leftHandSide rel)
   n <- match integerView b
   guard (n > 0 && hasNoVar (rightHandSide rel))
   let expr = cleanUpExpr (root (rightHandSide rel) (fromIntegral n))
       new = constructor rel a expr
       opp = constructor (flipSides rel) a (-expr)
       rt  = relationType rel
   return $ if odd n 
            then Logic.Var new 
            else if rt `elem` [LessThan, LessThanOrEqualTo]
                 then Logic.Var new :&&: Logic.Var opp
                 else Logic.Var new :||: Logic.Var opp

highRel2 :: Logic (Relation Expr) -> Maybe (String, Intervals Q)
highRel2 p = do
   xs <- switch (fmap highRel p)
   let vs = map fst (crush xs)
   guard (null vs || all (==head vs) vs)
   return (head vs, logicIntervals (fmap snd xs))

highRel :: Relation Expr -> Maybe (String, Intervals Q)
highRel rel = msum 
   [ cuTimes rel >>= highRel
   , cuPower rel >>= highRel2
   , cuPlus rel >>= highRel
   , quadrRel rel 
   ]

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
   
flipGT :: Relation a -> Relation a
flipGT r 
   | relationType r == GreaterThan = 
        rightHandSide r .<. leftHandSide r
   | relationType r == GreaterThanOrEqualTo = 
        rightHandSide r .<=. leftHandSide r 
   | otherwise = r

-- for similarity 
simLogic :: Ord a => (a -> a) -> Logic a -> Logic a -> Bool
simLogic f a b = rec (fmap f a) (fmap f b)
 where
   rec a b   
      | isOr a =
           let collect = nub . sort . trueOr . collectOr
           in recList (collect a) (collect b)
      | isAnd a =
           let collect = nub . sort . falseAnd . collectAnd
           in recList (collect a) (collect b)
      | otherwise = 
           shallowEq a b && recList (children a) (children b)
 
   recList xs ys = 
      length xs == length ys && and (zipWith rec xs ys) 
 
   collectOr (p :||: q) = collectOr p ++ collectOr q
   collectOr F = []
   collectOr a = [a]
   
   trueOr xs = if T `elem` xs then [] else xs
   
   collectAnd (p :&&: q) = collectAnd p ++ collectAnd q
   collectAnd T = []
   collectAnd a = [a]
   
   falseAnd xs = if F `elem` xs then [] else xs
   
   shallowEq a b = 
      let f = descend (const T) 
      in f a == f b 
      
   isOr (_ :||: _) = True
   isOr _          = False
   
   isAnd (_ :||: _) = True
   isAnd _          = False
   
   
eqAfterSubstitution :: (Functor f, Functor g) 
   => (f (g Expr) -> f (g Expr) -> Bool) -> Context (f (g Expr)) -> Context (f (g Expr)) -> Bool
eqAfterSubstitution eq ca cb = fromMaybe False $ do 
   a <- fromContext ca
   b <- fromContext cb
   let f = maybe id (fmap . fmap . substitute) . substOnClipboard
   return (f ca a `eq` f cb b)

substitute :: (String, Expr) -> Expr -> Expr
substitute (s, a) (Var b) | s==b = a
substitute pair expr = descend (substitute pair) expr

substOnClipboard :: Context a -> Maybe (String, Expr)
substOnClipboard = evalCM $ const $ do
   eq <- lookupClipboardG "subst"
   case eq of
      Var s :==: a -> return (s, a)
      _            -> fail "not a substitution"