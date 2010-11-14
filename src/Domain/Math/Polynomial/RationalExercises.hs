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
module Domain.Math.Polynomial.RationalExercises 
   ( rationalEquationExercise
   , simplifyRationalExercise, divisionRationalExercise
   , eqSimplifyRational
   ) where

import Common.Classes
import Common.Context
import Common.Exercise
import Common.Navigator
import Common.Rewriting
import Common.Strategy hiding (not)
import Common.Uniplate
import Common.View
import Control.Monad
import Data.List hiding (repeat, replicate)
import Data.Maybe
import Domain.Logic.Formula hiding (disjunctions, Var)
import qualified Domain.Logic as Logic
import qualified Domain.Logic.Views as Logic
import Domain.Logic.Views hiding (simplify)
import Domain.Math.Clipboard
import Domain.Math.Data.OrList
import Domain.Math.Data.Relation
import Domain.Math.Equation.CoverUpRules
import Domain.Math.Equation.Views
import Domain.Math.Examples.DWO4
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import Domain.Math.Polynomial.CleanUp
import Domain.Math.Polynomial.Exercises (eqOrList)
import Domain.Math.Polynomial.LeastCommonMultiple
import Domain.Math.Polynomial.RationalRules
import Domain.Math.Polynomial.Rules
import Domain.Math.Polynomial.Strategies
import Domain.Math.Polynomial.Views
import Domain.Math.SquareRoot.Views
import Domain.Math.Power.OldViews
import Prelude hiding (repeat, replicate, until, (^))
import qualified Data.Set as S

rationalEquationExercise :: Exercise (OrList (Equation Expr))
rationalEquationExercise = makeExercise 
   { exerciseId    = describe "solve a rational equation (with a variable in a divisor)" $ 
                        newId "algebra.equations.rational"
   , status        = Provisional
   , parser        = parseExprWith (pOrList (pEquation pExpr))
   , isSuitable    = isJust . rationalEquations
   , isReady       = solvedRelations
   , eqWithContext = Just eqRationalEquation
   , similarity    = eqOrList cleanUpExpr
   , strategy      = rationalEquationStrategy
   , ruleOrdering  = ruleOrderingWithId quadraticRuleOrder
   , navigation    = termNavigator
   , examples      = map return (concat brokenEquations)
   }
   
simplifyRationalExercise :: Exercise Expr
simplifyRationalExercise = makeExercise
   { exerciseId    = describe "simplify a rational expression (with a variable in a divisor)" $ 
                        newId "algebra.manipulation.rational.simplify"
   , status        = Alpha -- Provisional
   , parser        = parseExpr
-- isSuitable
   , isReady       = simplifiedRational
   -- , eqWithContext = Just eqSimplifyRational
   , similarity    = \x y -> cleanUpExpr x == cleanUpExpr y
   , strategy      = simplifyRationalStrategy
   , ruleOrdering  = ruleOrderingWithId quadraticRuleOrder
   , navigation    = termNavigator
   , examples      = concat (normBroken ++ normBroken2)
   }
   
divisionRationalExercise :: Exercise Expr
divisionRationalExercise = simplifyRationalExercise
   { exerciseId   = describe "divide a rational expression ('uitdelen')" $ 
                       newId "math.divrational"
   , strategy     = label "divide broken fraction" succeed
   , examples     = concat deelUit
   }

rationalEquationStrategy :: LabeledStrategy (Context (OrList (Equation Expr)))
rationalEquationStrategy = cleanUpStrategy (applyTop (fmap (fmap cleaner))) $
   label "Rational equation" $ 
       brokenFormToPoly <*> higherDegreeStrategyG <*> checkSolutionStrategy
 where
   -- a custom-made clean-up function. (Standard) cleanUpExpr function 
   -- has some strange interaction with the rules
   cleaner = transform (simplify (powerFactorViewWith rationalView)) 
           . cleanUpSimple . transform smart
   
   brokenFormToPoly = label "rational form to polynomial" $ until allArePoly $
      (  useC divisionIsZero <|> useC divisionIsOne 
     <|> useC sameDivisor <|> useC sameDividend
     <|> use coverUpPlus <|> use coverUpMinusLeft <|> use coverUpMinusRight
     <|> use coverUpNegate
      ) |>    
      (  useC crossMultiply <|> useC multiplyOneDiv  )
   checkSolutionStrategy = label "check solutions" $ 
      try (multi (showId checkSolution) (somewhere checkSolution))

allArePoly :: Context (OrList (Equation Expr)) -> Bool
allArePoly = 
   let f a = a `belongsTo` polyView
   in maybe False (all f . concatMap crush . crush) .  fromContext

simplifyRationalStrategy :: LabeledStrategy (Context Expr)
simplifyRationalStrategy = cleanUpStrategy (applyTop cleaner) $
   label "Simplify rational expression" $
      phaseOneDiv <*> phaseSimplerDiv
 where
   -- a custom-made clean-up function. (Standard) cleanUpExpr function 
   -- has some strange interaction with the rules
   cleaner = transform (simplify (powerFactorViewWith rationalView)) . cleanUpSimple
 
   phaseOneDiv = label "Write as division" $
      until isDivC $ 
         use fractionPlus <|> use fractionScale <|> use turnIntoFraction
   phaseSimplerDiv = label "Simplify division" $
      repeat $
         (onlyLowerDiv findFactorsStrategyG <|> somewhere (useC cancelTermsDiv)
            <|> commit (onlyUpperDiv (repeat findFactorsStrategyG) <*> useC cancelTermsDiv))
         |> ( somewhere (use merge) 
         <|> multi (showId distributeTimes) (exceptLowerDiv (use distributeTimes))
          )

isDivC :: Context a -> Bool
isDivC = maybe False (isJust . isDivide :: Term -> Bool) . currentT

-- First check that the whole strategy can be executed. Cleaning up is not 
-- propagated correctly to predicate in check combinator, hence the use of
-- cleanUpStrategy (which is not desirable here).
commit :: IsStrategy f => f (Context Expr) -> Strategy (Context Expr)
commit s = let cs  = cleanUpStrategy (applyTop cleanUpExpr) (label "" s)
               f a = fromMaybe a (do b <- top a; c <- current a; return (change (const c) b))
           in check (applicable cs . f) <*> s

exceptLowerDiv :: IsStrategy f => f (Context a) -> Strategy (Context a)
exceptLowerDiv = somewhereWith "except-lower-div" $ \a -> 
   if isDivC a then [1] else [0 .. arity a-1]

onlyUpperDiv :: IsStrategy f => f (Context a) -> Strategy (Context a)
onlyUpperDiv = onceWith "only-upper-div" $ \a -> [ 1 | isDivC a ]
   
onlyLowerDiv :: IsStrategy f => f (Context a) -> Strategy (Context a)
onlyLowerDiv = onceWith "only-lower-div" $ \a -> [ 2 | isDivC a ]
   
simplifiedRational :: Expr -> Bool
simplifiedRational expr =
   case expr of
      Negate a -> simplifiedRational a
      _        -> f expr
 where
   f (a :/: b) = inPolyForm a && noCommonFactor a b && inFactorForm b
   f _ = False

   inPolyForm :: Expr -> Bool
   inPolyForm expr =
      expr `belongsTo` polyNormalForm identity ||
      S.size (varSet expr) > 1
          
   inFactorForm :: Expr -> Bool
   inFactorForm = flip belongsTo $
      let v = first (polyNormalForm identity >>> second linearPolyView)
      in powerProductView >>> second (listView v)

rationalEquations :: OrList (Equation Expr) -> Maybe (OrList Expr)
rationalEquations = maybe (return true) f . disjunctions
 where 
   f xs = do 
      yss <- mapM rationalEquation xs
      return (join (orList yss))
 
rationalEquation :: Equation Expr -> Maybe (OrList Expr)
rationalEquation eq = do
   let (lhs :==: rhs) = coverUp eq
       (a, b, c) = rationalExpr (lhs .-. rhs)
   (_, as) <- match productView a
   (_, bs) <- match productView b
   let condition = foldr ((.&&.) . notZero) c bs
   new1    <- match higherDegreeEquationsView $ orList $ map (:==: 0) as
   return (restrictOrList condition new1)

restrictOrList :: Logic (Relation Expr) -> OrList Expr -> OrList Expr
restrictOrList p0 = maybe true (orList . filter check) . disjunctions
 where
   check zeroExpr = 
      case coverUp (zeroExpr :==: 0) of 
         Var x :==: a -> -- returns true if a contradiction was not found
            substVar x (cleanUpExpr a) p0 /= F 
         _ -> True

   substVar x a = Logic.simplify . catLogic . fmap (simpler . fmap (cleanUpExpr . subst))
    where 
      subst (Var s) | x == s = a
      subst expr = descend subst expr
       
   simpler r = fromMaybe (Logic.Var r) $ do
      a <- match (squareRootViewWith rationalView) (leftHandSide r)
      b <- match (squareRootViewWith rationalView) (rightHandSide r)
      case (a==b, relationType r) of
         (True,  EqualTo)    -> return T
         (False, EqualTo)    -> return F 
         (True,  NotEqualTo) -> return F
         (False, NotEqualTo) -> return T
         _ -> Nothing

eqRationalEquation :: Context (OrList (Equation Expr)) -> Context (OrList (Equation Expr)) -> Bool
eqRationalEquation ca cb = fromMaybe False $
   liftM2 (==) (solve ca) (solve cb)
 where
   solve ctx = do 
      let f = fromMaybe T . conditionOnClipboard
      a  <- fromContext ctx 
      xs <- rationalEquations a
      ys <- disjunctions (restrictOrList (f ctx) xs)
      return (sort (nub ys))
   
eqSimplifyRational :: Context Expr -> Context Expr -> Bool
eqSimplifyRational ca cb = fromMaybe False $ do
   a <- fromContext ca
   b <- fromContext cb
   let t1@(a1, a2, pa) = rationalExpr a
       t2@(b1, b2, pb) = rationalExpr b
       a1c = cleanUpExpr a1
       b1c = cleanUpExpr b1
       manyVars = S.size (varSet a `S.union` varSet b) > 1
   if manyVars then return True else do
   p1 <- match (polyViewWith rationalView) a1c
   p2 <- match (polyViewWith rationalView) b1c
   return (manyVars || p1==p2)

q = lcmExpr  (x^2+4*x-5) (x^2+5*x-6)
 where x = Var "x"
   
conditionOnClipboard :: Context a -> Maybe (Logic (Relation Expr))
conditionOnClipboard = evalCM $ const $
   lookupClipboardG "condition"

-- write expression as a/b, under certain conditions
rationalExpr :: Expr -> (Expr, Expr, Logic (Relation Expr))
rationalExpr expr =
   case expr of
      a :+: b  -> rationalExpr a `fPlus` rationalExpr b
      a :-: b  -> rationalExpr (a :+: Negate b)
      Negate a -> fNeg (rationalExpr a)
      a :*: b  -> rationalExpr a `fTimes` rationalExpr b
      a :/: b  -> rationalExpr a `fTimes` fRecip (rationalExpr b)
      Sym s [a, b] | isPowerSymbol s -> 
         fPower (rationalExpr a) b
      _ -> (expr, 1, T)
 where
   fNeg   (a, b, p)   = (neg a, b, p)
   fRecip (a, b, p)   = (b, a, notZero b .&&. p)
   fPower (a, b, p) n = (a .^. n, b .^. n, p)
   fTimes (a1, a2, p) (b1, b2, q) = (a1 .*. b1, a2 .*. b2, p .&&. q)
   fPlus  (a1, a2, p) (b1, b2, q) =
      case (divisionExpr c2 a2, divisionExpr c2 b2) of 
         (Just a3, Just b3) 
            | a1 == b1     -> (a1 .*. (a3 .+. b3), c2, pq)
            | a1 == neg b1 -> (a1 .*. (a3 .-. b3), c2, pq)
            | otherwise    -> (a1 .*. a3 .+. b1 .*. b3, c2, pq)
         _ -> (a1 .*. b2 .+. b1 .*. a2, a2 .*. b2, pq)
    where
      c2 = lcmExpr a2 b2
      pq = p .&&. q

notZero :: Expr -> Logic (Relation Expr)
notZero expr =
   case match rationalView expr of
      Just r | r /= 0    -> T
             | otherwise -> F
      _ -> Logic.Var (expr ./=. 0)

-----------------
-- test code

{-
raar = brokenExpr $ x^2/(5*x+6) + 1
 where x = Var "x"
-
go0 = checkExercise rationalEquationExercise

go = checkExercise simplifyRationalExercise

see n = printDerivation ex (examples ex !! (n-1))
 where ex = --rationalEquationExercise  
            simplifyRationalExercise
      
go4 = printDerivation findFactorsExercise $ -a + 4
 where x = Var "x"
       a = Var "a"
       
test = e4
 where 
   a  = Var "a"
   b  = Var "b"
   
   e1 = 6*a*b*a
   e2 = -4*b^2*a*2
   e3 = lcmExpr e1 e2
   e4 = divisionExpr e3 e1
   e5 = divisionExpr e3 e2
   
go = putStrLn $ unlines $ map show $ zip [1..] $ map (brokenEq []) (concat brokenEquations)
-}