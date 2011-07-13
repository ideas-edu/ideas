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
import Data.Maybe
import Domain.Math.Safe
import Domain.Math.CleanUp
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
   , testGenerator = Just $ liftM2 (\a b -> cleaner (singleton (a :==: b))) (sized linearGen) (sized linearGen)
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
   , getId collect
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
cleaner = join . fmap (trivial . fmap (fixpoint (transform f)))
 where
   f (Nat 1 :*: a) = a -- currently not removed by views
   f (a :*: Nat 1) = a
   f (Nat 0 :+: a) = a
   f (a :+: Nat 0) = a
   f (a :/: Negate b) = Negate (a/b)
   f (Negate a :/: b) = Negate (a/b)
   f (Negate (Negate a)) = a
   f (a :/: Nat 1) = a
   f e@(a :/: (b :/: c)) = 
      case (match rationalView b, match rationalView c) of
         (Just rb, Just rc) | b/=0 && c/=0 && c/=1 -> (c/b)*a
         _ -> e
   f ((a :/: b) :*: (c :/: d)) = (a*c)/(b*d)
   f e = fromMaybe e $
      canonical rationalView e
    `mplus` do
      let g xs | length xs > 1 = return (assocPlus rationalView xs)
          g _ = Nothing
      canonicalWithM g simpleSumView e
    `mplus` do
      let g (b, xs) | length xs > 1 = return (b, assocTimes rationalView xs)
          g _ = Nothing
      canonicalWithM g simpleProductView e

trivial :: Equation Expr -> WithBool (Equation Expr)
trivial eq@(lhs :==: rhs) =
   case (match rationalView lhs, match rationalView rhs) of
      (Just r1, Just r2)
         | r1 == r2   -> true
         | otherwise  -> false
      _  | lhs == rhs -> true
         | otherwise  -> singleton eq

balanceStrategy :: LabeledStrategy (Context (WithBool (Equation Expr)))
balanceStrategy = cleanUpStrategyAfter (applyTop cleaner) $
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
removeDivision :: Rule (Equation Expr) -- !!!!!!!!!!!!!!! TEMP solution
removeDivision = doAfter (fmap (cleanUpExpr . distributeLocal)) $
   describe "remove division" $ 
   makeRule (linbal, "remove-div") $ useRecognizer isTimesT $
   supply1 "factor" removeDivisionArg timesT 
 where
   removeDivisionArg (lhs :==: rhs) = do
      xs <- match simpleSumView lhs
      ys <- match simpleSumView rhs
      -- also consider parts without variables
      -- (but at least one participant should have a variable)
      zs <- forM (xs ++ ys) $ \a -> do
               (_, list) <- match simpleProductView a
               return [ (hasSomeVar a, e) | e <- list ]
      let f (b, e) = do 
             (_, this) <- match (divView >>> second integerView) e
             return (b, this)
          (bs, ns) = unzip (mapMaybe f (concat zs))
      let result = foldr1 lcm ns
      guard (or bs && result > 1)
      return (fromInteger result)

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
collect = makeSimpleRule (linbal, "collect") $ checkForChange $
   Just . fmap collectGlobal
   
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
collectLocal = simplifyWith mergeAlikeSum simpleSumView

collectGlobal :: Expr -> Expr
collectGlobal = fixpoint (transform collectLocal)

distributeLocal :: Expr -> Expr
distributeLocal expr =
   let (b, xs) = from simpleProductView expr 
       yss     = map (from simpleSumView) xs
       f a = build simpleProductView (False, a)
       zss | b = map neg (head yss) : tail yss
           | otherwise = yss
   in to simpleSumView (map (timesFraction . f) (combine zss))

 where
   -- (a*b)/c => (a/c)*b   (where a and c are rational)
   timesFraction :: Expr -> Expr
   timesFraction this = fromMaybe this $ do 
      ((a, b), c) <- match mv this
      guard (a /= 1 && hasSomeVar b)
      return $ fromRational (a/c) .*. b

   -- (Rat*b)/Rat
   mv = divView >>> (timesView >>> first rationalView) *** rationalView

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
   return $ simplifyWith (map f) simpleSumView a

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
   
{-
maf = let x=Var "x" in -2-x/3 :==: -4/35*x/3 
                       -- -2-(-x)/(-3) :==: x/(-3)/(5/2)/(7/2) 
                       -- x/1 :==: (-1)*x
                       
go = printDerivation balanceExercise (singleton maf) -}