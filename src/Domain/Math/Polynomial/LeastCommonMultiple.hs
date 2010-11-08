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
module Domain.Math.Polynomial.LeastCommonMultiple 
   ( lcmExpr, divisionExpr, noCommonFactor, equalFactors, testLCM 
   , powerProductView
   ) where

import Prelude hiding ((^))
import Common.TestSuite
import Common.View
import Control.Monad
import Data.List
import Data.Ratio
import Data.Maybe
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import Domain.Math.Power.Views
import Test.QuickCheck

-- | Returns the least common multiple of two expressions. 
lcmExpr :: Expr -> Expr -> Expr
lcmExpr a b = fromMaybe (a*b) $ do
   (ar, as) <- match powerProductView a
   (br, bs) <- match powerProductView b
   return $ build powerProductView (lcmR ar br, merge as bs)
 where   
   lcmR :: Rational -> Rational -> Rational
   lcmR r1 r2 = 
      let f r = numerator r * denominator r
      in fromIntegral (lcm (f r1) (f r2))
   
   merge :: [(Expr, Integer)] -> [(Expr, Integer)] -> [(Expr, Integer)]
   merge = foldr op id
    where
      op (e, n1) f ys = 
         let n2   = fromMaybe 0 (lookup e ys)
             rest = filter ((/=e) . fst) ys
         in (e, n1 `max` n2) : f rest

-- | Only succeeds if there is no remainder
divisionExpr :: Expr -> Expr -> Maybe Expr
divisionExpr a b = do
   (ar, as) <- match powerProductView a
   (br, bs) <- match powerProductView b
   xs       <- as `without` bs
   return $ build powerProductView (ar/br, xs)
 where
   without :: [(Expr, Integer)] -> [(Expr, Integer)] -> Maybe [(Expr, Integer)]
   without [] ys =
      guard (null ys) >> return []
   without ((e,n1):xs) ys = 
      let n2   = fromMaybe 0 (lookup e ys)
          rest = filter ((/=e) . fst) ys
      in liftM ((e,n1-n2):) (without xs rest)
      
powerProductView :: View Expr (Rational, [(Expr, Integer)])
powerProductView = makeView f g
 where
   f expr = do
      (b, xs) <- match productView expr
      let (r, ys) = collect xs
      return (if b then -r else r, merge ys)
         
   g (r, xs) =
      build productView (False, fromRational r : map (build pvn) xs)
   
   pvn :: View Expr (Expr, Integer)
   pvn = powerView >>> second integerView

   collect :: [Expr] -> (Rational, [(Expr, Integer)])
   collect = foldr op (1, [])
    where
      op e (r, xs) = 
         let mr   = match rationalView e 
             f r2 = (r*r2, xs)
             pair = fromMaybe (e,1) (match pvn e)
         in maybe (r, pair:xs) f mr

   merge :: [(Expr, Integer)] -> [(Expr, Integer)]
   merge [] = []
   merge xs@((e, _) : _) = 
      let (xs1, xs2) = partition ((==e) . fst) xs
          n = sum (map snd xs1) 
      in (e, n) : merge xs2
   
testLCM :: TestSuite
testLCM = suite "lcmExpr" $ do
   addProperty "transitivity" $ f3 $ \a b c -> 
      lcmExpr a (lcmExpr b c) ~= lcmExpr (lcmExpr a b) c
   addProperty "commutativity" $ f2 $ \a b -> 
      lcmExpr a b ~= lcmExpr b a
   addProperty "idempotency" $ f1 $ \a -> 
      lcmExpr a a ~= absExpr a
   addProperty "zero" $ f1 $ \a -> 
      lcmExpr a 0 ~= 0
   addProperty "one" $ f1 $ \a -> 
      lcmExpr a 1 ~= absExpr a
   addProperty "sign" $ f2 $ \a b -> 
      lcmExpr a b ~= lcmExpr (-a) b
 where 
   f1 g = liftM  g genExpr
   f2 g = liftM2 g genExpr genExpr
   f3 g = liftM3 g genExpr genExpr genExpr
 
   genExpr, genTerm, genAtom :: Gen Expr
   genExpr = do
      n  <- choose (0, 10)
      b  <- arbitrary
      xs <- replicateM n genTerm
      return $ build productView (b, xs)
   
   genTerm = frequency [(3, genAtom), (1, liftM fromInteger arbitrary)]
   
   genAtom = do
      v <- oneof $ map (return . Var) ["a", "b", "c"]
      i <- choose (-10, 10)
      n <- choose (0, 10)
      p <- frequency [(3, return v), (1, return (v .+. fromInteger i))]
      frequency [(3, return p), (1, return (p^fromInteger n))]

   (~=)    = equalFactors
   absExpr = simplifyWith (first (const False)) productView

noCommonFactor :: Expr -> Expr -> Bool
noCommonFactor x y = lcmExpr x y `equalFactors` (x*y)
   
equalFactors :: Expr -> Expr -> Bool
equalFactors x y = f x == f y
 where f = simplifyWith (second sort) powerProductView 