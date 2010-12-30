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

module Domain.Math.Power.Equation.NormViews where

import Common.Classes
import Common.View
import Common.Rewriting hiding (rewrite)
import Control.Arrow ( (>>^) )
import Control.Monad
import Data.List
import Data.Maybe
import Data.Ratio
import Domain.Math.Approximation
import Domain.Math.Data.OrList
import Domain.Math.Data.PrimeFactors
import Domain.Math.Data.Relation
import Domain.Math.Expr hiding ( (^) )
import Domain.Math.Numeric.Views
import Domain.Math.Polynomial.CleanUp
import Domain.Math.Polynomial.Views
import Domain.Math.Power.NormViews
import Domain.Math.Power.Utils
import Domain.Math.Power.Views
import Domain.Math.Simplification hiding (simplify, simplifyWith)


normPowerEqApproxView :: Int -> View (Relation Expr) (Expr, Expr)
normPowerEqApproxView d = makeView f (uncurry (.~=.))
   where
     f rel = case relationType rel of 
      EqualTo       -> fmap (second (simplifyWith (precision d) doubleView)) 
                     $ match (equationView >>> normPowerEqView) rel 
      Approximately -> return (leftHandSide rel, rightHandSide rel)
      _             -> Nothing

normPowerEqView :: View (Equation Expr) (Expr, Expr) -- with x>0!
normPowerEqView = makeView f (uncurry (:==:))
  where
    f expr = do
      -- selected var to the left, the rest to the right
      (lhs :==: rhs) <- varLeft expr >>= constRight
      -- match power
      (c, ax)        <- match (timesView <&> (identity >>^ (,) 1)) $
                          simplify normPowerView lhs
      (a, x)         <- match myPowerView ax
      -- simplify, scale and take root
      guard $ c /= 0 && x /= 0
      let y = cleanUpExpr $ (rhs ./. c) .^. (1 ./. x)
      return (a, simplify rationalView y)

    myPowerView =  powerView 
               <&> (rootView >>> second (makeView (\a->Just (1 ./. a)) (1 ./.)))
               <&> (identity >>^ \a->(a,1))

normPowerEqView' :: View (OrList (Equation Expr)) (OrList (Equation Expr))
normPowerEqView' = makeView (h. g . switch . (fmap f)) id
  where
    h = liftM $ fmap $ fmap cleanUpExpr
    g = liftM (transformOrList $ tryRewriteAll simplerPower)
    f expr = do
      -- selected var to the left, the rest to the right
      (lhs :==: rhs) <- varLeft expr >>= constRight
      -- match power
      (c, (a, x))    <- match unitPowerView lhs
      -- simplify, scale and take root
      let y = cleanUpExpr $ (rhs ./. c) .^. (1 ./. x)
      return $ a :==: simplify rationalView y

constRight :: Equation Expr -> Maybe (Equation Expr)
constRight (lhs :==: rhs) = do
  (vs, cs) <- fmap (partition hasSomeVar) (match sumView lhs)
  let rhs' = rhs .+. build sumView (map neg cs)
  return $ negateEq $ build sumView vs :==: simplifyWith mergeAlikeSum sumView rhs'

negateEq :: Equation Expr -> Equation Expr
negateEq (lhs :==: rhs) = 
  case lhs of
    Negate lhs' -> lhs' :==: neg rhs
    _           -> lhs  :==: rhs

varLeft :: Equation Expr -> Maybe (Equation Expr)
varLeft (lhs :==: rhs) = do
  (vs, cs) <- fmap (partition hasSomeVar) (match sumView rhs)
  return $ lhs .+. build sumView (map neg vs) :==: build sumView cs

scaleLeft :: Equation Expr -> Maybe (Equation Expr)
scaleLeft (lhs :==: rhs) = 
  match timesView lhs >>= \(c, x) -> return $ 
    x :==: simplifyWith (second mergeAlikeProduct) productView (rhs ./. c)

normExpEqView :: View (Equation Expr) (String, Rational)
normExpEqView = makeView f id >>> linearEquationView
  where
    try g a = fromMaybe a $ g a
    f e = do
      let (l :==: r) = try scaleLeft $ try constRight e
      return $ case match powerView l of
        Just (b, x) -> x :==: simplify normLogView (logBase b r)
        Nothing     -> l :==: r

normLogEqView :: View (OrList (Equation Expr)) (OrList (Equation Expr))
normLogEqView = makeView (liftM g . switch . fmap f) id
  where
    f expr@(lhs :==: rhs) = return $
      case match logView lhs of
        Just (b, x) -> x :==: b .^. rhs
        Nothing     -> expr
    g = normalize . fmap (fmap cleanUpExpr) . simplify normPowerEqView'
      . simplify higherDegreeEquationsView 

normLogView :: View Expr Expr
normLogView = makeView g id
  where
    g expr = 
      case expr of 
        Sym s [x, y] 
          | isLogSymbol s -> do
              b <- match integerView x
              let divExp (be, n) = return $ f be y ./. fromInteger n
              maybe (Just $ f b y) divExp $ greatestPower b
          | otherwise -> Nothing
        _ -> Nothing
    f b expr= 
      case expr of
        Nat 1         -> 0
        Nat n     
          | n == b    -> 1
          | otherwise -> maybe (logBase (fromInteger b) (fromInteger n)) fromInteger 
                       $ lookup b (allPowers n)
        e1 :*: e2 -> f b e1 .+. f b e2
        e1 :/: e2 -> f b e1 .-. f b e2
        Sqrt e    -> f b (e .^. (1 ./. 2))
        Negate e  -> Negate $ f b e
        Sym s [x,y]
          | isPowerSymbol s -> y .*. f b x
          | isRootSymbol  s -> f b (x .^. (1 ./. y))
        _         -> expr

simplerPower :: Expr -> [Expr]
simplerPower = rec
  where
    rec expr = 
      case expr of      
        Sqrt x -> rec $ Sym powerSymbol [x, 1 / 2]
        Sym s [x, y]
          | isRootSymbol s  -> rec $ Sym powerSymbol [x, 1 / y]
          | isPowerSymbol s -> f
          | otherwise -> []
            where f | y == 0 = [1]
                    | y == 1 = [x]
                    | x == 0 = [0]
                    | otherwise =
                      -- geheel getal
                      liftM fromRational (matchM rationalView expr)
                      `mplus`
                      -- breuk
                      do 
                        ry <- matchM rationalView y
                        rx <- matchM rationalView x
                        guard $ denominator rx == 1 && denominator ry /= 1
                        map fromInteger $ 
                          takeRoot (numerator rx ^ numerator ry) (denominator ry)
                      `mplus`
                      -- (a/b)^y -> a^x/b^y
                      do
                        (a, b) <- matchM divView x
                        return $ build divView (a .^. y, b .^. y)
        _ -> []

