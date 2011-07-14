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
module Domain.Math.Polynomial.Balance (balanceExercise) where

import Common.Library
import Common.Utils.Uniplate
import Common.Utils (fixpoint, safeHead)
import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import Data.Ratio
import Domain.Math.Safe
import Domain.Math.Data.Polynomial
import Domain.Math.Data.WithBool
import Domain.Math.Data.Relation
import Domain.Math.Equation.BalanceRules
import Domain.Math.Equation.Views
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import Domain.Math.Polynomial.Examples
import Domain.Math.Polynomial.Generators
import Domain.Math.Polynomial.Rules (conditionVarsRHS, flipEquation)
import Domain.Math.Polynomial.Views
import Domain.Math.Simplification (mergeAlikeSum)
import qualified Data.Traversable as T
import Test.QuickCheck (sized)

------------------------------------------------------------
-- Exercise

balanceExercise :: Exercise (WithBool (Equation Expr))
balanceExercise = makeExercise 
   { exerciseId    = describe "Solve a linear equation using only balance rules." $ 
                     newId "algebra.equations.linear.balance"
   , status        = Provisional
   , parser        = parseBoolEqExpr
   , similarity    = withoutContext ((==) `on` cleaner)
   , equivalence   = withoutContext (viewEquivalent eqView)
   , suitable      = predicateView (traverseView linearEquationView)
   , ready         = predicateView (traverseView (equationSolvedWith mixedFractionNF))
                     <||> predicateView (traverseView (equationSolvedWith rationalNF))
                     <||> predicateView (traverseView (equationSolvedWith doubleNF))
   , strategy      = balanceStrategy
   , ruleOrdering  = ruleOrderingWithId balanceOrder
   , navigation    = termNavigator
   , testGenerator = Just $ liftM2 (\a b -> singleton (a :==: b)) (sized linearGen) (sized linearGen)
   , examples      = map (mapSecond singleton) linearExamples
   }

balanceOrder :: [Id]
balanceOrder = 
   [ getId removeDivision, getId collect
   , getId varRightMinus, getId varRightPlus
   , getId conLeftMinus, getId conLeftPlus
   , getId varLeftMinus, getId varLeftPlus
   , getId conRightMinus, getId conRightPlus
   , getId scaleToOne, getId flipEquation
   , getId divideCommonFactor, getId distribute
   , getId collect, getId divisionToFraction
   ]

eqView :: View (WithBool (Equation Expr)) (WithBool (String, Rational))
eqView = makeView (either (Just . fromBool) f . fromWithBool) (fmap g)
 where
   f (lhs :==: rhs) = do
      (s, p) <- match (polyViewWith rationalView) (lhs-rhs)
      case degree p of
         0 -> Just $ fromBool $ coefficient 0 p == 0
         1 -> Just $ singleton $ (s, - coefficient 0 p / coefficient 1 p)
         _ -> Nothing
   g (s, r) = Var s :==: fromRational r

------------------------------------------------------------
-- Strategy

cleaner :: WithBool (Equation Expr) -> WithBool (Equation Expr)
cleaner = join . fmap (trivial . fmap cleanerExpr)

cleanerExpr :: Expr -> Expr
cleanerExpr = transform f -- no fixpoint is needed
 where
   f (a :/: Nat 1) = f $ a
   f (a :/: Negate (Nat 1)) = f $ Negate a
   f (Negate a :/: Negate b) = f (a/b)
   f (a :/: Negate b) = f $ Negate (a/b)
   f (Negate a :/: b) = f $ Negate (a/b)
   f (Negate (Negate a)) = f $ a
   f e = cleanSum (cleanProduct (simplify rationalView e))
    
   cleanSum = 
      let g x y = canonical rationalView (x :+: y)
      in simplifyWith (adjacent g) simpleSumView

   cleanProduct = 
      let g x y = canonical rationalView (x :*: y)
      in simplifyWith (mapSecond (adjacent g)) simpleProductView

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

balanceStrategy :: LabeledStrategy (Context (WithBool (Equation Expr)))
balanceStrategy = cleanUpStrategyAfter (applyTop cleaner) $
   label "Balance equation" $
       label "Phase 1" (repeatS
           (  use collect
          <|> use distribute 
          <|> use removeDivision
          <|> somewhere (use divisionToFraction)
           ))
   <*> label "Phase 2" (repeatS
           (  use varLeftMinus <|> use varLeftPlus 
          <|> use conRightMinus <|> use conRightPlus 
          <|> (check p2 <*> (use varRightMinus <|> use varRightPlus)) 
          <|> (check p1 <*> (use conLeftMinus <|> use conLeftPlus)
           ))
       <*> try (use scaleToOne)
       <*> try (use calculate))
       -- flip sides of an equation (at most once)
   <%> try (atomic (use conditionVarsRHS <*> use flipEquation))
       -- divide by a common factor (but not as final "scale-to-one" step)
   <%> many (notS (use scaleToOne) <*> use divideCommonFactor)
 where
   -- move constants to left only if there are no variables on the left
   p1 = maybe False (either (const False) (hasNoVar . leftHandSide) . fromWithBool) . fromContext
   p2 ceq = fromMaybe False $ do
      wb           <- fromContext ceq
      lhs :==: rhs <- either (const Nothing) Just (fromWithBool wb)
      (x1, a, _)   <- matchLin lhs
      (x2, b, _)   <- matchLin rhs
      return (x1 == x2 && b > a && a /= 0)

------------------------------------------------------------
-- Rules

linbal :: String
linbal = "algebra.equations.linear.balance"

checkForChange :: (MonadPlus m, Eq a) => (a -> m a) -> a -> m a
checkForChange f a = mfilter (/= a) (f a)
  
calculate :: Rule (WithBool (Equation Expr))
calculate = makeSimpleRule (linbal, "calculate") $ checkForChange $
   Just . cleaner
  
-- factor is always positive due to lcm function
removeDivision :: Rule (Equation Expr)
removeDivision = doAfter (fmap distributeTimes) $
   describe "remove division" $ 
   makeRule (linbal, "remove-div") $ useRecognizer isTimesT $
   supply1 "factor" removeDivisionArg timesT 
 where
   removeDivisionArg (lhs :==: rhs) = do
      xs <- match simpleSumView lhs
      ys <- match simpleSumView rhs
      -- also consider parts without variables
      -- (but at least one participant should have a variable)
      zs <- mapM getFactor (xs++ys)
      let (b, result) = foldr op (False, 1) zs
          op (b1, a1) (b2, a2) = (b1 || b2, a1 `lcm` a2)
      guard (b && result > 1)
      return (fromInteger result)
      
   getFactor (Negate a) = getFactor a
   getFactor expr = do
      (b, c) <- match (divView >>> second integerView) expr
      return (hasSomeVar b, c)
    `mplus` do
      r <- match rationalView expr
      return (False, denominator r)
    `mplus` do 
      (r, c) <- match (timesView >>> first rationalView) expr
      return (hasSomeVar c, denominator r)
    `mplus` do 
      (b, r) <- match (timesView >>> second rationalView) expr
      return (hasSomeVar b, denominator r)
    `mplus` do
      (_, ps) <- match simpleProductView expr
      guard (any (`belongsTo` rationalView) ps)
      return (False, 1)
    `mplus` do
      guard (isVariable expr)
      return (False, 1)

divisionToFraction :: Rule Expr
divisionToFraction = 
   describe "turn a division into a multiplication with a fraction" $
   makeSimpleRule (linbal, "div-to-fraction") $ \expr -> do
      (a, r) <- match (divView >>> second rationalView) expr
      guard (hasSomeVar a && r /= 0)
      return (fromRational (1/r)*a)

divideCommonFactor :: Rule (Equation Expr)
divideCommonFactor = doAfter (fmap distributeDiv) $
   describe "divide by common factor" $ 
   makeRule (linbal, "smart-div") $ useRecognizer isTimesT $
   supply1 "factor" getArg divisionT
 where
   getArg (lhs :==: rhs) 
      | all (/=0) ns && n > 1 = Just (fromInteger n) 
      | otherwise             = Nothing
    where
       xs = from sumView lhs ++ from sumView rhs
       ns = map getFactor xs
       n  = foldr1 gcd ns
      
   getFactor expr
      | hasNoVar expr = fromMaybe 1 $ match integerView expr
      | otherwise = fromMaybe 1 $ do
           (a, b) <- match timesView expr
           case (match integerView a, match integerView b) of
              (Just n, _) | hasSomeVar b -> return n
              (_, Just n) | hasSomeVar a -> return n
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
scaleToOne = doAfter (fmap distributeDiv) $ 
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
collect = makeSimpleRule (linbal, "collect") $ 
   -- don't use this rule just for cleaning up
   checkForChange (Just . fmap collectGlobal) . fmap cleanerExpr 
   
distribute :: Rule (Equation Expr)
distribute = makeSimpleRule (linbal, "distribute") $ checkForChange $
   Just . fmap (fixpoint f)
 where
   f (a :*: (b :+: c))  = f (a*b+a*c)
   f (a :*: (b :-: c))  = f (a*b-a*c)
   f ((a :+: b) :*: c)  = f (a*c+b*c)
   f ((a :-: b) :*: c)  = f (a*c-b*c)
   f (Negate (a :+: b)) = f (-a-b) 
   f (Negate (a :-: b)) = f (-a+b)
   f (Negate (Negate a)) = f a  
   f (a :-: (b :+: c)) = f (a-b-c)
   f (a :-: (b :-: c)) = f (a-b+c)
   f (a :-: Negate b)  = f (a+b)
   f a = descend f a

matchLin :: Expr -> Maybe (String, Rational, Rational)
matchLin expr = do
   (s, p) <- match (polyNormalForm rationalView) expr
   guard (degree p < 2)
   return (s, coefficient 1 p, coefficient 0 p)
   
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

-- for debugging                 
{-
go = printDerivation balanceExercise $ singleton $ let x=Var "x" in
   (x+2+7/2*x)/(3/2) :==: -3/2*x/4*0 -}

adjacent :: (a -> a -> Maybe a) -> [a] -> [a]
adjacent f = rec
 where
   rec (x:y:rest) = 
      case f x y of
         Just xy -> rec (xy:rest)
         Nothing -> x:rec (y:rest)
   rec xs = xs