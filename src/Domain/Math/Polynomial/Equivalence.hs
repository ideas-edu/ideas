{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
   , eqAfterSubstitution, intervalRelations
   ) where

import Common.Context
import Common.Algebra.Boolean
import Common.Rewriting
import Common.Uniplate
import Common.View
import Control.Monad
import qualified Data.Traversable as T
import Data.Maybe
import Data.Ord
import Domain.Logic.Formula hiding (Var)
import Domain.Math.Data.Interval
import Domain.Math.Data.Polynomial hiding (eval)
import Domain.Math.Data.Relation hiding (eval)
import Domain.Math.Data.SquareRoot
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import Domain.Math.CleanUp
import Domain.Math.Polynomial.Views
import Domain.Math.SquareRoot.Views
import Prelude hiding ((^), sqrt)
import qualified Domain.Logic.Formula as Logic
import qualified Domain.Logic.Generator as Logic

relationInterval :: Ord a => RelationType -> a -> Interval a
relationInterval relType = 
   case relType of
      EqualTo              -> point
      NotEqualTo           -> except
      LessThan             -> lessThan
      GreaterThan          -> greaterThan
      LessThanOrEqualTo    -> lessThanOrEqualTo
      GreaterThanOrEqualTo -> greaterThanOrEqualTo
      Approximately        -> point -- i.e., equalTo

intervalRelations :: Eq a => a -> Interval a -> Logic (Relation a)
intervalRelations v = ors . map (ands . map Logic.Var . make) . segments
 where
   make pair = 
      case pair of
         (Unbounded, Unbounded)   -> []
         (Unbounded, Including b) -> [v .<=. b]
         (Unbounded, Excluding b) -> [v .<. b]
         (Including a, Unbounded) -> [v .>=. a]
         (Excluding a, Unbounded) -> [v .>. a]
         (Including a, Including b) 
            | a == b    -> [v .==. a]
            | otherwise -> [v .>=. a, v .<=. b]
         (Including a, Excluding b) -> [v .>=. a, v .<. b]
         (Excluding a, Including b) -> [v .>. a, v .<=. b]
         (Excluding a, Excluding b) -> [v .>. a, v .<. b]

logicInterval :: Ord a => Logic (Interval a) -> Interval a
logicInterval = 
   foldLogic (id, implies, equivalent, intersect, union, complement, true, false)

-----------------------------------------------------------
             
linEq :: Relation Expr -> Relation Expr -> Bool
linEq a b = fromMaybe False $ liftM2 (==) (linRel a) (linRel b)

linRel :: Relation Expr -> Maybe (String, Interval Rational)
linRel = linRelWith rationalView

linRelWith :: (Ord a, Fractional a)
           => View Expr a -> Relation Expr -> Maybe (String, Interval a)
linRelWith v rel =
   case match (linearViewWith v) (lhs - rhs) of
      Nothing -> do
         (s, p) <- match (polyViewWith v) (lhs - rhs)
         guard (degree p == 0)
         let list = case compare (coefficient 0 p) 0 of
                       LT -> [LessThan, LessThanOrEqualTo]
                       EQ -> [EqualTo, LessThanOrEqualTo, GreaterThanOrEqualTo]
                       GT -> [GreaterThan, GreaterThanOrEqualTo]
         return (s, fromBool $ relationType rel `elem` list)
      Just (s, a, b) 
         | a==0 -> 
              return (s, fromBool (b==0))
         | otherwise -> do
              let zero = -b/a
                  tp = relationType $ (if a<0 then flipSides else id) rel
              return (s, relationInterval tp zero) 
 where
   lhs = leftHandSide rel
   rhs = rightHandSide rel

newtype Q = Q (SquareRoot Rational) deriving (Show, Eq, Num, Fractional)

-- Use normal (numeric) ordering on square roots
instance Ord Q where
   Q a `compare` Q b = comparing f a b
    where
      f :: SquareRoot Rational -> Double
      f = eval . fmap fromRational

qView :: View (SquareRoot Rational) Q
qView = makeView (return . Q) (\(Q a) -> a)

quadrEqContext :: Context (Logic (Relation Expr)) -> Context (Logic (Relation Expr)) -> Bool
quadrEqContext = eqContextWith (polyEq quadrRel)

highEqContext :: Context (Logic (Relation Expr)) -> Context (Logic (Relation Expr)) -> Bool
highEqContext = eqContextWith (polyEq highRel)

eqContextWith :: (Logic (Relation Expr) -> Logic (Relation Expr) -> Bool)
              -> Context (Logic (Relation Expr)) 
              -> Context (Logic (Relation Expr))
              -> Bool
eqContextWith eq a b = isJust $ do
   termA <- fromContext a
   termB <- fromContext b
   guard $ 
      case (ineqOnClipboard a, ineqOnClipboard b) of 
         (Just x,  Just y)  -> eq x y && eq termA termB
         (Just x,  Nothing) -> eq (fmap toEq x) termA && eq x termB
         (Nothing, Just y)  -> eq (fmap toEq y) termB && eq termA y
         (Nothing, Nothing) -> eq termA termB
 where
   toEq :: Relation Expr -> Relation Expr
   toEq r = leftHandSide r .==. rightHandSide r

ineqOnClipboard :: Context a -> Maybe (Logic (Relation Expr))
ineqOnClipboard = evalCM $ const $ do
   expr <- lookupClipboard "ineq"
   fromExpr expr

polyEq :: (Relation Expr -> Maybe (String, Interval Q)) -> Logic (Relation Expr) -> Logic (Relation Expr) -> Bool
polyEq f p q = fromMaybe False $ do
   xs <- T.mapM f p
   ys <- T.mapM f q
   let vs = map fst (varsLogic xs ++ varsLogic ys)
   guard (null vs || all (==head vs) vs)
   let ix = logicInterval (fmap snd xs)
       iy = logicInterval (fmap snd ys)
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

highRel2 :: Logic (Relation Expr) -> Maybe (String, Interval Q)
highRel2 p = do
   xs <- T.mapM highRel p
   let vs = map fst (varsLogic xs)
   guard (null vs || all (==head vs) vs)
   return (head vs, logicInterval (fmap snd xs))

highRel :: Relation Expr -> Maybe (String, Interval Q)
highRel rel = msum 
   [ cuTimes rel >>= highRel
   , cuPower rel >>= highRel2
   , cuPlus rel >>= highRel
   , quadrRel rel 
   ]

quadrRel :: Relation Expr -> Maybe (String, Interval Q)
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
            LT -> fromBool $ tp `elem` [NotEqualTo, GreaterThan, GreaterThanOrEqualTo]
            EQ | tp `elem` [EqualTo, Approximately, LessThanOrEqualTo] -> 
                    point pa
               | tp == NotEqualTo           -> except pa
               | tp == LessThan             -> false
               | tp == GreaterThan          -> except pa
               | tp == GreaterThanOrEqualTo -> true
            GT | tp `elem` [EqualTo, Approximately] -> 
                    point pa <||> point pb
               | tp == NotEqualTo -> 
                    except pa `intersect` except pb
               | tp == LessThan -> 
                    open pa pb
               | tp == LessThanOrEqualTo ->
                    closed pa pb
               | tp == GreaterThan -> 
                    lessThan pa <||> greaterThan pb
               | tp == GreaterThanOrEqualTo ->
                    lessThanOrEqualTo pa <||> greaterThanOrEqualTo pb
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
simLogic f p0 q0 = Logic.equalLogicACI (fmap f p0) (fmap f q0)
   
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