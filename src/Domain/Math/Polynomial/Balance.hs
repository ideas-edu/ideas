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
module Domain.Math.Polynomial.Balance (balanceExercise) where

import Common.Library
import Common.Uniplate
import Common.Utils (fixpoint, safeHead)
import Control.Monad
import Data.Function
import Data.Maybe
import Domain.Math.CleanUp
import Domain.Math.Data.Polynomial
import Domain.Math.Data.Relation
import Domain.Math.Equation.BalanceRules
import Domain.Math.Equation.Views
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import Domain.Math.Polynomial.Examples
import Domain.Math.Polynomial.Views
import Domain.Math.Simplification
import qualified Data.Traversable as T

------------------------------------------------------------
-- Exercise

balanceExercise :: Exercise (Equation Expr)
balanceExercise = makeExercise 
   { exerciseId   = describe "Solve a linear equation using only balance rules." $ 
                    newId "algebra.equations.linear.balance"
   , status       = Provisional
   , parser       = parseEqExpr
   , similarity   = withoutContext ((==) `on` fmap cleanUpExpr)
   , equivalence  = withoutContext (viewEquivalent linearEquationView)
   , isSuitable   = (`belongsTo` linearEquationView)
   , isReady      = solvedRelationWith $ \a -> 
                       a `belongsTo` mixedFractionNormalForm || 
                       a `belongsTo` rationalNormalForm
   , strategy     = linearStrategy
   , ruleOrdering = ruleOrderingWithId [getId removeDivision]
   , navigation   = termNavigator
   , examples     = linearExamples
   }
      
------------------------------------------------------------
-- Strategy

linearStrategy :: LabeledStrategy (Context (Equation Expr))
linearStrategy = cleanUpStrategy (applyTop (fmap cleanUpExpr)) $
   label "Linear Equation" $
       label "Phase 1" (repeatS (
               use collect
           |>  (use distribute <|> use removeDivision)))
   <*> label "Phase 2" (repeatS (
              use varToLeft <|> use conToRight)
          <*> try (use scaleToOne))

------------------------------------------------------------
-- Rules

linbal :: String
linbal = "algebra.equations.linear.balance"
  
-- factor is always positive due to lcm function
removeDivision :: Rule (Equation Expr)
removeDivision =
   describe "remove division" $ 
   makeRule (linbal, "remove-div") $ useRecognizer isTimesT $
   supply1 "factor" removeDivisionArg timesT 
 where
   removeDivisionArg (lhs :==: rhs) = do
      xs <- match sumView lhs
      ys <- match sumView rhs
      -- also consider parts without variables
      -- (but at least one participant should have a variable)
      zs <- forM (xs ++ ys) $ \a -> do
               (_, list) <- match productView a
               return [ (hasSomeVar a, e) | e <- list ]
      let f (b, e) = do 
             (_, this) <- match (divView >>> second integerView) e
             return (b, this)
          (bs, ns) = unzip (mapMaybe f (concat zs))
      guard (or bs)
      return (fromInteger (foldr1 lcm ns))

varToLeft :: Rule (Equation Expr)
varToLeft = doAfter (fmap collectLocal) $
   makeRule (linbal, "var-left") $ useRecognizer isPlusT $
   supply1 "term" varToLeftArg minusT
 where
    varToLeftArg :: Equation Expr -> Maybe Expr
    varToLeftArg (_ :==: rhs) = do
       (x, a, _) <- matchLin rhs
       guard (a /= 0)
       return (fromRational a .*. Var x)

conToRight :: Rule (Equation Expr)
conToRight = doAfter (fmap collectLocal) $
   makeRule (linbal, "con-to-right") $ useRecognizer isPlusT $
   supply1 "term" conToRightArg minusT
 where
    conToRightArg :: Equation Expr -> Maybe Expr
    conToRightArg (lhs :==: _) = do
       (_, _, b) <- matchLin lhs
       guard (b /= 0)
       return (fromRational b)

scaleToOne :: Rule (Equation Expr)
scaleToOne = doAfter (fmap distributeLocal) $ 
   makeRule (linbal, "scale-to-one") $ useRecognizer isTimesT $ 
   supply1 "factor" scaleToOneArg divisionT
 where
   scaleToOneArg :: Equation Expr -> Maybe Expr
   scaleToOneArg (lhs :==: rhs) = do
      (_, a1, b1) <- matchLin lhs
      guard (a1 /= 0 && a1 /= 1 && b1 == 0 && hasNoVar rhs)
      return (fromRational a1)
      
collect :: Rule (Equation Expr)
collect = makeSimpleRule (linbal, "collect") $ \old -> do
   let new = fmap collectGlobal old
   guard (old /= new)
   return new
   
distribute :: Rule (Equation Expr)
distribute = makeSimpleRule (linbal, "distribute") $ \old -> do
   let new = fmap distributeGlobal old
   guard (old /= new)
   return new
   
matchLin :: Expr -> Maybe (String, Rational, Rational)
matchLin expr = do
   (s, p) <- match (polyNormalForm rationalView) expr
   guard (degree p < 2)
   return (s, coefficient 1 p, coefficient 0 p)
   
------------------------------------------------------------
-- Helpers

collectLocal :: Expr -> Expr
collectLocal = collectLikeTerms

collectGlobal :: Expr -> Expr
collectGlobal = fixpoint (transform collectLocal)

distributeLocal :: Expr -> Expr
distributeLocal expr = fromMaybe expr $ do
   (b, xs) <- matchM productView expr 
   yss     <- mapM (matchM sumView) xs
   let f a = build productView (False, a)
       zss | b = map neg (head yss) : tail yss
           | otherwise = yss
   return $ build sumView (map f (combine zss))

distributeGlobal :: Expr -> Expr
distributeGlobal = fixpoint (transform distributeLocal)

combine :: [[a]] -> [[a]]
combine = foldr (\x xs -> [ a:as | a <- x, as <- xs ]) [[]]

isPlusT :: Equation Expr -> Equation Expr -> Bool
isPlusT old new = isJust (diffPlus old new)

diffPlus :: Equation Expr -> Equation Expr -> Maybe Expr
diffPlus old new = do
   let myView = polyViewWith rationalView
       f = liftM snd . matchM myView
   a1 :==: a2 <- T.mapM f old
   b1 :==: b2 <- T.mapM f new
   x <- safeHead (vars (leftHandSide old) ++ vars (rightHandSide old))
   let d1 = b1 - a1
       d2 = b2 - a2
   guard (d1 == d2)
   return (build myView (x, d1))

isTimesT :: Equation Expr -> Equation Expr -> Bool
isTimesT old new = isJust (diffTimes old new)

diffTimes :: Equation Expr -> Equation Expr -> Maybe Expr
diffTimes old new = do
   let myView = polyViewWith rationalView
       f = liftM snd . matchM myView
   a1 :==: a2 <- T.mapM f old
   b1 :==: b2 <- T.mapM f new
   x  <- safeHead (vars (leftHandSide old) ++ vars (rightHandSide old))
   d1 <- b1 `division` a1
   d2 <- b2 `division` a2
   guard (d1 == d2)
   return (build myView (x, d1))
  
------------------------------------------------------------
-- Debug code
   
{-
main :: IO ()
main = mapM_ go [0 .. length (examples balanceExercise) - 1]

go :: Int -> IO ()
go n = printDerivation balanceExercise (snd $ examples balanceExercise !! n)

ok = checkExercise balanceExercise

ff a = printDerivation balanceExercise a

x = Var "x"

r = diagnose (emptyState ex old) new
 where
   ex  = balanceExercise
   old = 3*x + 1 :==: 4
   new = 3*x + 2 :==: 5
   
testje = diffTimes old new
 where
   old = (x+1)/2 :==: (1/3)*(1-x)
   new = 3*(x+1) :==: 2*(1-x)
-}