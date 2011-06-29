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
import Data.Either
import Data.Function
import Data.Maybe
import Domain.Math.Safe
import Domain.Math.CleanUp
import Domain.Math.Data.Polynomial
import Domain.Math.Data.Relation
import Domain.Math.Equation.BalanceRules
import Domain.Math.Equation.Views
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import Domain.Math.Polynomial.Examples
import Domain.Math.Polynomial.Rules (conditionVarsRHS, flipEquation)
import Domain.Math.Polynomial.Views
import Domain.Math.Simplification (collectLikeTerms)
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
   , suitable     = predicateView linearEquationView
   , ready        = predicateView (equationSolvedWith mixedFractionNF)
                    <||> predicateView (equationSolvedWith rationalNF)
                    <||> predicateView (equationSolvedWith doubleNF)
   , strategy     = balanceStrategy
   , ruleOrdering = ruleOrderingWithId balanceOrder
   , navigation   = termNavigator
   , examples     = linearExamples
   }

balanceOrder :: [Id]
balanceOrder = 
   [ getId collect, getId removeDivision, getId collect
   , getId varRightMinus, getId varRightPlus
   , getId conLeftMinus, getId conLeftPlus
   , getId varLeftMinus, getId varLeftPlus
   , getId conRightMinus, getId conRightPlus
   , getId scaleToOne, getId flipEquation
   , getId divideCommonFactor, getId distribute
   ]
      
------------------------------------------------------------
-- Strategy

balanceStrategy :: LabeledStrategy (Context (Equation Expr))
balanceStrategy = cleanUpStrategyAfter (applyTop (fmap cleanUpExpr)) $
   label "Balance equation" $
       label "Phase 1" (repeatS 
           (  use collect
          <|> use distribute 
          <|> use removeDivision
           ))
   <*> label "Phase 2" (repeatS 
           (  use varLeftMinus <|> use varLeftPlus 
          <|> use conRightMinus <|> use conRightPlus 
          <|> (check p2 <*> (use varRightMinus <|> use varRightPlus)) 
          <|> (check p1 <*> (use conLeftMinus <|> use conLeftPlus)
           ))
       <*> try (use scaleToOne))
       -- flip sides of an equation (at most once)
   <%> option (atomic (use conditionVarsRHS <*> use flipEquation))
       -- divide by a common factor (but not as final "scale-to-one" step)
   <%> many (notS (use scaleToOne) <*> use divideCommonFactor)
 where
   -- move constants to left only if there are no variables on the left
   p1 = maybe False (hasNoVar . leftHandSide) . fromContext
   p2 ceq = fromMaybe False $ do
      lhs :==: rhs <- fromContext ceq
      (x1, a, _) <- matchLin lhs
      (x2, b, _) <- matchLin rhs
      return (x1 == x2 && b > a && a /= 0)

------------------------------------------------------------
-- Rules

linbal :: String
linbal = "algebra.equations.linear.balance"
  
-- factor is always positive due to lcm function
removeDivision :: Rule (Equation Expr)
removeDivision = doAfter (fmap distributeLocal) $
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

divideCommonFactor :: Rule (Equation Expr)
divideCommonFactor = doAfter (fmap distributeDiv) $
   describe "divide by common factor" $ 
   makeRule (linbal, "smart-div") $ useRecognizer isTimesT $
   supply1 "factor" getArg divisionT
 where
   getArg (lhs :==: rhs) = do 
      xs <- match sumView lhs
      ys <- match sumView rhs
      ps <- mapM getFactor (xs ++ ys)
      let (ns, ms) = partitionEithers ps
          n = if null ns then 1 else minimum ns
          p = (==0) . (`mod` n)
      guard (all p (ns ++ ms))
      return (fromInteger n)
      
   getFactor expr
      | hasNoVar expr = do 
           n <- match integerView expr
           return (Right n)
      | otherwise = do
           (a, b) <- match timesView expr
           case (match integerView a, match integerView b) of
              (Just n, _) | hasSomeVar b -> return (Left n)
              (_, Just n) | hasSomeVar a -> return (Left n)
              _ -> Nothing

varLeftMinus, varLeftPlus :: Rule (Equation Expr)
varLeftMinus = varLeft True  (linbal, "var-left-minus")
varLeftPlus  = varLeft False (linbal, "var-left-plus")

varLeft :: IsId a => Bool -> a -> Rule (Equation Expr)
varLeft useMinus rid = doAfter (fmap collectLocal) $
   makeRule rid $ useRecognizer isPlusT $
   supply1 "term" varLeftArg (if useMinus then minusT else plusT)
 where
    varLeftArg :: Equation Expr -> Maybe Expr
    varLeftArg (lhs :==: rhs) = do
       guard (hasSomeVar lhs)
       (x, a, _) <- matchLin rhs
       guard (if useMinus then a > 0 else a < 0)
       return (fromRational (abs a) .*. Var x)

conRightMinus, conRightPlus :: Rule (Equation Expr)
conRightMinus = conRight True  (linbal, "con-right-minus")
conRightPlus  = conRight False (linbal, "con-right-plus")

conRight :: IsId a => Bool -> a -> Rule (Equation Expr)
conRight useMinus rid = doAfter (fmap collectLocal) $
   makeRule rid $ useRecognizer isPlusT $
   supply1 "term" conRightArg (if useMinus then minusT else plusT)
 where
    conRightArg :: Equation Expr -> Maybe Expr
    conRightArg (lhs :==: _) = do
       guard (hasSomeVar lhs)
       (_, _, b) <- matchLin lhs
       guard (if useMinus then b > 0 else b < 0)
       return (fromRational (abs b))

varRightMinus, varRightPlus :: Rule (Equation Expr)
varRightMinus = flipped (linbal, "var-right-minus") varLeftMinus
varRightPlus  = flipped (linbal, "var-right-plus")  varLeftPlus

conLeftMinus, conLeftPlus :: Rule (Equation Expr)
conLeftMinus = flipped (linbal, "con-left-minus") conRightMinus
conLeftPlus  = flipped (linbal, "con-left-plus")  conRightPlus

flipped :: IsId a => a -> Rule (Equation b) -> Rule (Equation b)
flipped rid = liftRule flipView . changeId (const (newId rid))
 where flipView = makeView (Just . flipSides) flipSides

scaleToOne :: Rule (Equation Expr)
scaleToOne = doAfter (fmap distributeLocal) $ 
   makeRule (linbal, "scale-to-one") $ useRecognizer isTimesT $ 
   supply1 "factor" scaleToOneArg divisionT
 where
   scaleToOneArg :: Equation Expr -> Maybe Expr
   scaleToOneArg (lhs :==: rhs) = f lhs rhs `mplus` f rhs lhs
      
   f :: Expr -> Expr -> Maybe Expr
   f expr c = do
      (_, a1, b1) <- matchLin expr
      guard (a1 /= 0 && a1 /= 1 && b1 == 0 && hasNoVar c)
      return (fromRational a1)
      
collect :: Rule (Equation Expr)
collect = makeSimpleRule (linbal, "collect") $ \old -> do
   let norm = fmap cleanUpExpr old -- don't use rule just for cleaning up
       new  = fmap collectGlobal norm
   guard (norm /= new)
   return new
   
distribute :: Rule (Equation Expr)
distribute = makeSimpleRule (linbal, "distribute") $ \old -> do
   let norm = fmap cleanUpExpr old -- don't use rule just for cleaning up
       new  = fmap distributeGlobal norm
   guard (norm /= new)
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
   return $ build sumView (map (timesFraction . f) (combine zss))

distributeGlobal :: Expr -> Expr
distributeGlobal = fixpoint (transform distributeLocal)

distributeDiv :: Expr -> Expr
distributeDiv expr = fromMaybe expr $ do
   (a, b) <- match (divView >>> second rationalView) expr
   let f x = fromMaybe (x/fromRational b) $ do
          (y, z) <- match (timesView >>> first rationalView) x
          new    <- y `safeDiv` b
          return (fromRational new * z)
        `mplus` do
          (y, z) <- match (timesView >>> second rationalView) x
          new    <- z `safeDiv` b
          return (y * fromRational new)
   return $ simplifyWith (map f) sumView a

-- (a*b)/c => (a/c)*b   (where a and c are rational)
timesFraction :: Expr -> Expr
timesFraction expr = fromMaybe expr $ do 
   ((a, b), c) <- match mv expr
   guard (a /= 1 && hasSomeVar b)
   return $ fromRational (a/c) .*. b
 where
   -- (Rat*b)/Rat
   mv = divView >>> (timesView >>> first rationalView) *** rationalView

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
   d1 <- b1 `safeDiv` a1
   d2 <- b2 `safeDiv` a2
   guard (d1 == d2)
   return (build myView (x, d1))