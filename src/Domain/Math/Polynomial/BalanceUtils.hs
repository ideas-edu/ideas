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
module Domain.Math.Polynomial.BalanceUtils
   ( eqView, minusView, negView
   , matchLin, matchPlusCon
   , cleaner, cleanerExpr
   , linbal, checkForChange
   , termArg, factorArg, factorArgs
   , buggyBalanceRule, buggyBalanceRuleArgs
   -- , buggyBalanceRewriteRule
   , buggyBalanceExprRule, buggyBalanceRecognizer
   , collectLocal, collectGlobal
   , distributeDiv, distributeTimes
   , isPlusT, diffPlus
   , isTimesT, diffTimes
   ) where

import Common.Library hiding (rewrite)
import Common.Utils (fixpoint)
import Common.Utils.Uniplate
import Control.Monad
import Data.List
import Data.Maybe
import Data.Monoid
import Domain.Math.Data.Polynomial
import Domain.Math.Data.Relation
import Domain.Math.Data.WithBool
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import Domain.Math.Polynomial.Views
import Domain.Math.Safe
import Domain.Math.Simplification (mergeAlikeSum)

eqView :: View (WithBool (Equation Expr)) (WithBool (String, Rational))
eqView = makeView (either (Just . fromBool) f . fromWithBool) (fmap g)
 where
   f (lhs :==: rhs) = do
      (s, p) <- match (polyViewWith rationalView) (lhs-rhs)
      case degree p of
         0 -> Just $ fromBool $ coefficient 0 p == 0
         1 -> Just $ singleton (s, - coefficient 0 p / coefficient 1 p)
         _ -> Nothing
   g (s, r) = Var s :==: fromRational r

minusView :: View Expr (Expr, Expr)
minusView = makeView isMinus (uncurry (:-:))

negView :: View Expr Expr
negView = makeView isNegate Negate

matchLin :: Expr -> Maybe (Expr, Rational, Rational)
matchLin expr = do
   (s, p) <- match (polyNormalForm rationalView) expr
   guard (degree p == 1)
   return (Var s, coefficient 1 p, coefficient 0 p)

matchPlusCon :: Expr -> Maybe (Expr, Rational)
matchPlusCon expr =
   match (plusView >>> second rationalView) expr
 `mplus`
   match (plusView >>> toView swapView >>> second rationalView) expr

------------------------------------------------------------
-- Strategy

cleaner :: WithBool (Equation Expr) -> WithBool (Equation Expr)
cleaner = join . fmap (trivial . fmap cleanerExpr)

cleanerExpr :: Expr -> Expr
cleanerExpr = transform f -- no fixpoint is needed
 where
   f (a :/: Nat 1) = f a
   f (a :/: Negate (Nat 1)) = f $ Negate a
   f (Negate a :/: Negate b) = f (a/b)
   f (a :/: Negate b) = f $ Negate (a/b)
   f (Negate a :/: b) = f $ Negate (a/b)
   f (Negate (Negate a)) = f a
   f e = cleanSum (cleanProduct (simplify rationalView e))

   cleanSum =
      let g x y = canonical rationalView (x :+: y)
      in simplifyWith (adjacent g) simpleSumView

   cleanProduct =
      let g x y = canonical rationalView (x :*: y)
      in simplifyWith (mapSecond (adjacent g)) simpleProductView

adjacent :: (a -> a -> Maybe a) -> [a] -> [a]
adjacent f = rec
 where
   rec (x:y:rest) =
      case f x y of
         Just xy -> rec (xy:rest)
         Nothing -> x:rec (y:rest)
   rec xs = xs

trivial :: Equation Expr -> WithBool (Equation Expr)
trivial eq@(lhs :==: rhs) =
   case (match rationalView lhs, match rationalView rhs) of
      (Just r1, Just r2)
         | r1 == r2                -> true
         | otherwise               -> false
      _  | any nonsense [lhs, rhs] -> false
         | lhs == rhs              -> true
         | otherwise               -> singleton eq

nonsense :: Expr -> Bool
nonsense = any p . universe
 where
   p (_ :/: a) = maybe False (==0) (match rationalView a)
   p _         = False

------------------------------------------------------------
-- Arguments

termArg :: Expr -> Environment
termArg expr = singleBinding ("term" .<-. expr)

factorArg :: Expr -> Environment
factorArg expr = singleBinding ("factor" .<-. expr)

factorArgs :: [Expr] -> Environment
factorArgs =
   let f a b = Typed $ ("factor" ++ show a) .<-. b
   in makeEnvironment . zipWith f [1::Int ..]

------------------------------------------------------------
-- Rules

linbal :: Id
linbal = newId "algebra.equations.linear.balance"

bugbal :: IsId n => n -> Id
bugbal n = newId (linbal, "buggy", n)

checkForChange :: (MonadPlus m, Eq a) => (a -> m a) -> a -> m a
checkForChange f a = f a >>= \b -> guard (a /= b) >> return b

buggyBalanceRule :: IsId n => n -> (Equation Expr -> Maybe (Equation Expr)) -> Rule (Equation Expr)
buggyBalanceRule n f = useEquality eq $ buggyRule $ 
   makeRule (bugbal n) $ makeTrans f
 where
   eq = viewEquivalent (traverseView (polyViewWith rationalView))

buggyBalanceRuleArgs :: IsId n => n -> (Equation Expr -> Maybe (Equation Expr, Environment)) -> Rule (Equation Expr)
buggyBalanceRuleArgs n f = useEquality eq $ buggyRule $ 
   makeRule (bugbal n) $ makeEnvTrans g
 where
   eq = viewEquivalent (traverseView (polyViewWith rationalView))
   g a = case f a of
            Just (b, env) -> changeEnvironment (mappend env) >> return b
            Nothing -> mempty

buggyBalanceExprRule :: IsId n => n -> (Expr -> Maybe Expr) -> Rule Expr
buggyBalanceExprRule n f = 
   buggyRule $ makeSimpleRule (bugbal n) f

buggyBalanceRecognizer :: IsId n => n -> (a -> a -> Maybe Environment) -> Recognizer a
buggyBalanceRecognizer n p = 
   buggyRecognizer $ makeRecognizer (bugbal n) p

------------------------------------------------------------
-- Helpers

collectLocal :: Expr -> Expr
collectLocal = simplifyWith (mapSecond f) simpleProductView
             . simplifyWith mergeAlikeSum simpleSumView
 where
   f xs | length ys > 1 = ys++zs
        | otherwise     = xs
    where
      (ys, zs) = partition hasNoVar xs

collectGlobal :: Expr -> Expr
collectGlobal = fixpoint (transform collectLocal)

distributeDiv :: Expr -> Expr
distributeDiv expr = fromMaybe expr $ do
   (a, r) <- match (divView >>> second rationalView) expr
   return $ simplifyWith (fmap (`divide` r)) simpleSumView a
 where
   divide x r = fromMaybe (x/fromRational r) $ do
      (y, z) <- match (timesView >>> first rationalView) x
      new    <- y `safeDiv` r
      return (fromRational new * z)
    `mplus` do
      (y, z) <- match (timesView >>> second rationalView) x
      new    <- z `safeDiv` r
      return (y * fromRational new)

distributeTimes :: Expr -> Expr
distributeTimes expr = fromMaybe expr $ do
   (r, a) <- match (timesView >>> first rationalView) expr
              `mplus`
             match (timesView >>> second rationalView >>> toView swapView) expr
   return $ simplifyWith (fmap (times r)) simpleSumView a
 where
   times r x = fromMaybe (fromRational r*x) $ do
      (a, b) <- match (divView >>> second rationalView) x
      guard (b /= 0)
      return (fromRational (r/b) * a)

isPlusT :: Equation Expr -> Equation Expr -> Bool
isPlusT old new = isJust (diffPlusEq old new)

diffPlusEq :: Equation Expr -> Equation Expr -> Maybe Expr
diffPlusEq (a1 :==: a2) (b1 :==: b2) = do
   d1 <- diffPlus a1 b1
   d2 <- diffPlus a2 b2
   guard (d1 == d2)
   return d1

diffPlus :: Expr -> Expr -> Maybe Expr
diffPlus a b = do
   let myView = polyViewWith rationalView
   (x, pa) <- matchM myView a
   (y, pb) <- matchM myView b
   guard (x==y)
   let d = pb - pa
   return $ build myView (x, d)

isTimesT :: Equation Expr -> Equation Expr -> Bool
isTimesT old new = isJust (diffTimesEq old new)

diffTimesEq :: Equation Expr -> Equation Expr -> Maybe Expr
diffTimesEq (a1 :==: a2) (b1 :==: b2) = do
   d1 <- diffTimes a1 b1
   d2 <- diffTimes a2 b2
   guard (d1 == d2)
   return d1

diffTimes :: Expr -> Expr -> Maybe Expr
diffTimes a b = do
   let myView = polyViewWith rationalView
   (x, pa) <- matchM myView a
   (y, pb) <- matchM myView b
   guard (x==y)
   if pa==0 && pb==0 then return 1 else do
   d <- pb `safeDiv` pa
   return $ build myView (x, d)