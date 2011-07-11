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
module Domain.Math.CleanUp 
   ( cleanUpRelations, cleanUpRelation, cleanUpExpr
   , cleanUpSimple, cleanUpView, cleanUpACView
   , assocExpr, acExpr, smart, assocPlus
   ) where

import Common.Classes
import Common.Utils.Uniplate
import Common.Utils (fixpoint)
import Common.View
import Control.Monad
import Data.List
import Data.Maybe
import Data.Ord
import Data.Ratio
import Data.Foldable (foldMap)
import Domain.Math.Data.OrList
import Domain.Math.Data.Relation
import Domain.Math.Data.SquareRoot (fromSquareRoot)
import Domain.Math.Expr
import Domain.Math.Numeric.Views (rationalView, integerView)
import Domain.Math.Power.OldViews (powerFactorViewWith)
import Domain.Math.SquareRoot.Views (squareRootViewWith)
import Prelude hiding ((^), recip)
import qualified Prelude

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

------------------------------------------------------------
-- Cleaning up

cleanUpSimple :: Expr -> Expr
cleanUpSimple = fixpoint (transform (smart . f))
 where
   f = simplifyWith (assocPlus rationalView) sumView

cleanUpRelations :: OrList (Relation Expr) -> OrList (Relation Expr)
cleanUpRelations = noDuplicates . foldMap cleanUpRelation

cleanUpRelation :: Relation Expr -> OrList (Relation Expr)
cleanUpRelation = f . fmap cleanUpBU
 where
   f rel
      | any falsity (universe a ++ universe b) = false
      | a == b    = fromBool (relationType rel `elem` equals)
      | otherwise = 
           case (match rationalView a, match rationalView b) of
              (Just r, Just s) -> fromBool (eval (relationType rel) r s)
              _                -> singleton rel
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

cleanUpView, cleanUpACView :: View Expr Expr
cleanUpView   = makeView (return . cleanUpExpr) id
cleanUpACView = makeView (return . acExpr . cleanUpExpr) id

-- normalize expr with associativity and commutative rules for + and *
assocExpr, acExpr :: Expr -> Expr
assocExpr = normExpr id
acExpr    = normExpr sort
   
normExpr :: ([Expr] -> [Expr]) -> Expr -> Expr
normExpr f = rec 
 where
   rec expr = 
      case (from sumView expr, from productView expr) of
         (xs, _) | length xs > 1 -> 
            to sumView $ f $ map rec xs
         (_, (b, xs)) | length xs > 1 -> 
            to productView (b, f $ map rec xs)
         _ -> 
            descend rec expr 
  
------------------------------------------------------------
-- Associativity

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
    `mplus`
      liftM (transform smart) (canonical specialSqrtOrder e)
      -- Just simplify order of terms with square roots for now
    `mplus` do
      let f xs | length xs > 1 = return (assocPlus myView xs)
          f _ = Nothing 
      canonicalWithM f sumView e
    `mplus`
      canonical myView e
    `mplus` do
      let f (b, xs) | length xs > 1 = return (b, assocTimes myView xs)
          f _ = Nothing
      canonicalWithM f productView e
 where
   myView = powerFactorViewWith rationalView

specialSqrtOrder :: View Expr [Expr]
specialSqrtOrder = toView sumView >>> makeView f id
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
smart e = e