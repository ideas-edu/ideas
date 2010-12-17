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
module Domain.Math.Polynomial.CleanUp 
   ( cleanUpRelations, cleanUpRelation, cleanUpExpr
   , cleanUpSimple, collectLikeTerms
   , acExpr, smart
   ) where

import Common.Utils (fixpoint)
import Common.Uniplate
import Common.View
import Control.Monad
import Data.List
import Data.Maybe
import Data.Ord
import Domain.Math.Data.OrList
import Domain.Math.Data.Relation
import Domain.Math.Data.SquareRoot (fromSquareRoot)
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import Domain.Math.Power.OldViews
import Domain.Math.Simplification hiding (simplify, simplifyWith)
import Domain.Math.SquareRoot.Views
import Prelude hiding ((^), recip)
import qualified Prelude
import Data.Ratio

----------------------------------------------------------------------
-- Root simplification

simplerRoot :: Rational -> Integer -> Expr
simplerRoot a b 
   | b < 0          = 1 ./. simplerRoot a (abs b)
   | a < 0 && odd b = neg (simplerRoot (abs a) b)
   | otherwise      = f (numerator a) b ./. f (denominator a) b
 where
   f x y
      | x == 0              = 0
      | y == 0 || x <= 0    = root (fromIntegral x) (fromIntegral y)
      | e Prelude.^ y == x  = fromIntegral e
      | otherwise           = root (fromIntegral x) (fromIntegral y)
    where
      e = round ((fromIntegral x :: Double) ** (1 / fromIntegral y))

----------------------------------------------------------------------
-- Expr normalization

collectLikeTerms :: Expr -> Expr
collectLikeTerms = simplifyWith f sumView
 where
   f = mergeAlikeSum . map (simplifyWith (second mergeAlikeProduct) productView)

------------------------------------------------------------
-- Cleaning up

cleanUpSimple :: Expr -> Expr
cleanUpSimple = fixpoint (transform (f2 . f1))
 where
   use v = simplifyWith (assocPlus v) sumView
   f1 = use rationalView
   f2 = smartConstructors

cleanUpRelations :: OrList (Relation Expr) -> OrList (Relation Expr)
cleanUpRelations = idempotent . join . fmap cleanUpRelation

cleanUpRelation :: Relation Expr -> OrList (Relation Expr)
cleanUpRelation = f . fmap cleanUpBU
 where
   f rel
      | any falsity (universe a ++ universe b) = false
      | a == b    = fromBool (relationType rel `elem` equals)
      | otherwise = 
           case (match rationalView a, match rationalView b) of
              (Just r, Just s) -> fromBool (eval (relationType rel) r s)
              _                -> return rel
    where
      (a, b) = (leftHandSide rel, rightHandSide rel)

   equals = 
      [EqualTo, LessThanOrEqualTo, GreaterThanOrEqualTo, Approximately]

   falsity :: Expr -> Bool
   falsity (Sqrt e)  = maybe False (<0)  (match rationalView e)
   falsity (_ :/: e) = maybe False (==0) (match rationalView e)
   falsity _         = False
   
-- also simplify square roots
cleanUpExpr :: Expr -> Expr
cleanUpExpr = fixpoint $ 
   cleanUpBU . transform (simplify (squareRootViewWith rationalView))

-- normalize expr with associativity and commutative rules for + and *
acExpr :: Expr -> Expr
acExpr expr = 
   case (match sumView expr, match productView expr) of
      (Just xs, _) | length xs > 1 -> 
         build sumView $ sort $ map acExpr xs
      (_, Just (b, xs)) | length xs > 1 -> 
         build productView (b, sort $ map acExpr xs)
      _ -> 
         descend acExpr expr
   
------------------------------------------------------------
-- Technique 1: fixed points of views
{-
cleanUpFix :: Expr -> Expr
cleanUpFix = fixpoint (f4 . f3 . f2 . f1)
 where
   use v = transform (simplifyWith (assoPlus v) sumView)
 
   f1 = use rationalView
   f2 = use (squareRootViewWith rationalView)
   f3 = use (powerFactorViewWith rationalView)
   f4 = smartConstructors
-}
assocPlus, assocTimes :: View Expr a -> [Expr] -> [Expr]
assocPlus  = assocOp (+)
assocTimes = assocOp (*)

assocOp :: (Expr -> Expr -> Expr) -> View Expr a -> [Expr] -> [Expr]
assocOp op v = rec . map (simplify v)
 where
   rec (x:y:zs) =
      case canonical v (op x y) of
         Just a  -> rec (a:zs)
         Nothing -> x:rec (y:zs)
   rec xs = xs

------------------------------------------------------------
-- Fixpoint of a bottom-up traversal

cleanUpBU :: Expr -> Expr
cleanUpBU = {- fixpoint $ -} transform $ \e -> 
   simplify myView $ 
   fromMaybe (smart e) $
      canonical rationalView e
    `mplus` do
      a <- canonical specialSqrtOrder e
      -- Just simplify order of terms with square roots for now
      return (transform smart a)
    `mplus` do
      xs <- match sumView e
      guard (length xs > 1)
      return $ build sumView $
         assocPlus myView xs
    `mplus`
      canonical myView e
    `mplus` do
      (b, xs) <- match productView e
      guard (length xs > 1)
      return $ build productView 
         (b, assocTimes myView xs)
 where
   myView = powerFactorViewWith rationalView

specialSqrtOrder :: View Expr [Expr]
specialSqrtOrder = sumView >>> makeView f id
 where
   make = match (squareRootViewWith rationalView)
   g    = isNothing . fromSquareRoot . snd
   f xs = do
      ys <- mapM make xs
      return $ map fst $ sortBy (comparing g) $ zip xs ys

smart :: Expr -> Expr
smart (a :*: b) = a .*. b
smart (a :/: b) = a ./. b
smart expr@(Sym s [x, y]) 
   | isPowerSymbol s = x .^. y
   | isRootSymbol  s = fromMaybe expr $ 
        liftM2 simplerRoot (match rationalView x) (match integerView y)
smart (Negate a) = neg a
smart (a :+: b) = a .+. b
smart (a :-: b) = a .-. b
smart (Sqrt (Nat n)) = simplerRoot (fromIntegral n) 2
-- smart (Sqrt a)  = maybe (Sqrt a) (`simplerRoot` 2) (match rationalView a)
smart e = e


------------------------------------------------------------
-- Testing

{-
-- List with hard cases
hardCases = map cleanUpExpr $ let x=Var "x" in
  [ -1/2*x*(x/1)
  , (x/(-3))
  , (x/(-3))^2
  , (0-x)*(-x)/(-5/2)
  , (x/(-1))^2
  , (x/(-1))^2-(-7/2)*x/(-1)
  , (x^2+0)*3
  , -(49/9*x^2+0^2)*(3/16)
  , (0*x-(-x^2))*(-3)
  , x^2 - x^2
  , x^2-x^2-(x+x)*1
  , x^2/(16/3)-x^2*(-1/3)-(x+(-26/3)-x^2)*1
  , (-7+7*x)^2-(x*0)^2/(-3)
  , 1*(x+93)+4
  , (1*(x+(-93/5))-(-4+x/19))/8-(x^2-x+(19/2-x)-34/3*(x*(-41/2)))/9
  ] -}