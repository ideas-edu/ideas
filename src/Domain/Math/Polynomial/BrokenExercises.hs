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
module Domain.Math.Polynomial.BrokenExercises 
   ( brokenEquationExercise
   , normalizeBrokenExercise, divisionBrokenExercise
   ) where

import Prelude hiding (repeat, until, (^))
import Common.Context
import Common.Exercise
import Common.Navigator
import Common.Strategy hiding (not)
import Common.TestSuite
import Common.Transformation
import Common.Classes (crush)
import Common.Uniplate
import Common.View
import Control.Monad
import Domain.Math.Expr
import Domain.Math.Data.Relation
import Domain.Math.Data.OrList
import Domain.Math.Equation.CoverUpRules
import Domain.Math.Equation.Views
import Domain.Math.Examples.DWO4
import Domain.Math.Polynomial.CleanUp
import Domain.Math.Polynomial.Strategies
import Domain.Math.Polynomial.Exercises (eqOrList)
import Domain.Math.Polynomial.Views
import Domain.Math.Power.Views
import Domain.Math.Numeric.Views
import Data.Ratio
import Data.List hiding (repeat)
import Data.Maybe
import Test.QuickCheck hiding (label)

go  = checkExercise brokenEquationExercise
go2 = checkExercise normalizeBrokenExercise

brokenEquationExercise :: Exercise (OrList (Equation Expr))
brokenEquationExercise = makeExercise 
   { exerciseId   = describe "solve a broken equation (with a variable in a divisor)" $ 
                       newId "math.brokeneq"
   , status       = Alpha -- Provisional
   , parser       = parseExprWith (pOrList (pEquation pExpr))
   , isReady      = solvedRelations
--   , equivalence  = ??
   , similarity   = eqOrList cleanUpExpr2
   , strategy     = brokenEquationStrategy
   , navigation   = termNavigator
   , examples     = map return (concat brokenEquations)
   }
   
normalizeBrokenExercise :: Exercise Expr
normalizeBrokenExercise = makeExercise
   { exerciseId   = describe "normalize a broken expression (with a variable in a divisor)" $ 
                       newId "math.normbroken"
   , status       = Alpha -- Provisional
   , parser       = parseExpr
   , isReady      = isNormBroken
--   , equivalence  = ??
   , similarity   = \x y -> cleanUpExpr2 x == cleanUpExpr2 y
   , strategy     = normalizeBrokenStrategy
   , navigation   = termNavigator
   , examples     = concat (normBroken ++ normBroken2)
   }
   
divisionBrokenExercise :: Exercise Expr
divisionBrokenExercise = normalizeBrokenExercise
   { exerciseId   = describe "divide a broken fraction ('uitdelen')" $ 
                       newId "math.divbroken"
   , strategy     = label "divide broken fraction" succeed
   , examples     = concat deelUit
   }

brokenEquationStrategy :: LabeledStrategy (Context (OrList (Equation Expr)))
brokenEquationStrategy = cleanUpStrategy (applyTop (fmap (fmap cleanUpExpr2))) $
   label "Broken equation" $ 
       brokenFormToPoly <*> higherDegreeStrategyG
 where
   brokenFormToPoly = label "broken form to polynomial" $ until allArePoly $
      (  use divisionIsZero <|> use divisionIsOne 
     <|> use sameDivisor <|> use sameDividend
     <|> use coverUpPlus <|> use coverUpMinusLeft <|> use coverUpMinusRight
     <|> use coverUpNegate
      ) |>    
      (  use crossMultiply <|> use multiplyOneDiv  )

allArePoly :: Context (OrList (Equation Expr)) -> Bool
allArePoly = 
   let f a = a `belongsTo` polyView
   in maybe False (all f . concatMap crush . crush) .  fromContext

normalizeBrokenStrategy :: LabeledStrategy (Context Expr)
normalizeBrokenStrategy = cleanUpStrategy (applyTop cleanUpExpr2) $
   label "Normalize broken expression" $
      phaseOneDiv <*> phaseSimplerDiv
 where
   phaseOneDiv = label "Write as division" $
      until isDivC $ 
         use fractionPlus <|> use fractionScale <|> use turnIntoFraction
   phaseSimplerDiv = label "Simplify division" $
      repeat (use cancelTermsDiv)
       -- onceDiv findFactorsStrategyG
       
onceDiv :: IsStrategy f => f (Context a) -> Strategy (Context a)
onceDiv s = check isDivC <*> once s

isDivC :: Context a -> Bool
isDivC = maybe False (isJust . isDivide :: Expr -> Bool) . currentT

-- a/b = 0  iff  a=0 (and b/=0)
divisionIsZero :: Rule (Equation Expr)
divisionIsZero = makeSimpleRule "divisionIsZero" $ \(lhs :==: rhs) -> do
   guard (rhs == 0)
   (a, _) <- match divView lhs
   return (a :==: 0)
   
-- a/b = 1  iff  a=b (and b/=0)
divisionIsOne :: Rule (Equation Expr)
divisionIsOne = makeSimpleRule "divisionIsOne" $ \(lhs :==: rhs) -> do
   guard (rhs == 1)
   (a, b) <- match divView lhs
   return (a :==: b)

-- a/c = b/c  iff  a=b (and c/=0)
sameDivisor :: Rule (Equation Expr)
sameDivisor = makeSimpleRule "sameDivisor" $ \(lhs :==: rhs) -> do
   (a, c1) <- match divView lhs
   (b, c2) <- match divView rhs
   guard (c1==c2)
   return (a :==: b)
   
-- a/b = a/c  iff  a=0 or b=c (and b/=0 and c/=0)
sameDividend :: Rule (OrList (Equation Expr))
sameDividend = makeSimpleRule "sameDividend" $ oneDisjunct $ \(lhs :==: rhs) -> do
   (a1, b) <- match divView lhs
   (a2, c) <- match divView rhs
   guard (a1==a2)
   return $ orList [a1 :==: 0, b :==: c]
   
-- a/b = c/d  iff  a*d = b*c
crossMultiply :: Rule (Equation Expr)
crossMultiply = makeSimpleRule "crossMultiply" $ \(lhs :==: rhs) -> do
   (a, b) <- match divView lhs
   (c, d) <- match divView rhs
   return (a*d :==: b*c)
   
-- a/b = c  iff  a = b*c
multiplyOneDiv :: Rule (Equation Expr)
multiplyOneDiv = makeSimpleRuleList "multiplyOneDiv" $ \(lhs :==: rhs) -> 
   f (:==:) lhs rhs ++ f (flip (:==:)) rhs lhs
 where
   f eq ab c = do 
      guard (not (c `belongsTo` divView))
      (a, b) <- matchM divView ab
      return (a `eq` (b*c))
      
-- a/c + b/c = a+b/c   (also see Numeric.Rules)
fractionPlus :: Rule Expr -- also minus
fractionPlus = makeSimpleRule "fraction plus" $ \expr -> do
   ((a, b), (c, d)) <- match myView expr
   guard (b == d)
   return (build divView (a+c, b))
 where
   myView = plusView >>> (divView *** divView)

-- ab/ac  =>  b/c  (if a/=0)
cancelTermsDiv :: Rule Expr
cancelTermsDiv = liftRule myView $ 
   makeSimpleRule "cancel terms div" $ \((b, xs), (c, ys)) -> do
      let (ps, qs) = rec xs ys
      guard (length ps < length xs)
      return ((b, ps), (c, qs))
 where
   myView = divView >>> (productView *** productView)
   rec xs ys = foldr add (xs, []) ys
   add y (xs, ys) 
      | y `elem` xs = (delete y xs, ys)
      | otherwise   = (xs, y:ys)

fractionScale :: Rule Expr
fractionScale = liftRule myView $ 
   makeSimpleRule "fraction scale" $ \((a, e1), (b, e2)) -> do
      guard (e1 /= e2)
      let e3 = lcmExpr e1 e2
      ma <- divisionExpr e3 e1
      mb <- divisionExpr e3 e2
      guard (ma /= 1 || mb /= 1)
      return $ ((ma*a, e3), (mb*b, e3))
 where
   myView = plusView >>> (divView *** divView)
   
turnIntoFraction :: Rule Expr
turnIntoFraction = liftRule plusView $
   makeSimpleRule "turn into fraction" $ \(a, b) ->
      liftM (\c -> (c, b)) (f a b) `mplus` 
      liftM (\c -> (a, c)) (f b a)
 where
   f a b = do
      guard (not (a `belongsTo` divView))
      (_, e) <- match divView b
      return $ build divView (a*e, e)

isNormBroken :: Expr -> Bool
isNormBroken (a :/: b) = noVarInDivisor a && noVarInDivisor b
isNormBroken e = noVarInDivisor e

noVarInDivisor :: Expr -> Bool
noVarInDivisor expr = and [ noVars a | _ :/: a <- universe expr ]

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
   pvn = simplePowerView >>> second integerView

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

test = e4
 where 
   a  = Var "a"
   b  = Var "b"
   
   e1 = 6*a*b*a
   e2 = -4*b^2*a*2
   e3 = lcmExpr e1 e2
   e4 = divisionExpr e3 e1
   e5 = divisionExpr e3 e2
   
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

   x ~= y = let f = simplifyWith (second sort) powerProductView 
            in f x == f y
   absExpr = simplifyWith (first (const False)) productView