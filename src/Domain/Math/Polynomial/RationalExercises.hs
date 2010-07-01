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
   ) where

import Common.Classes
import Common.Context
import Common.Exercise
import Common.Navigator
import Common.Rewriting.Term (Term)
import Common.Strategy hiding (not)
import Common.Uniplate hiding (somewhere)
import Common.View
import Control.Monad
import Data.List hiding (repeat)
import Data.Maybe
import Domain.Logic.Formula (Logic)
import Domain.Logic.Views
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
import Prelude hiding (repeat, until, (^))

rationalEquationExercise :: Exercise (OrList (Equation Expr))
rationalEquationExercise = makeExercise 
   { exerciseId    = describe "solve a rational equation (with a variable in a divisor)" $ 
                        newId "math.rationaleq"
   , status        = Alpha -- Provisional
   , parser        = parseExprWith (pOrList (pEquation pExpr))
 -- isSuitable 
   , isReady       = solvedRelations
   , eqWithContext = Just eqRationalEquation
   , similarity    = eqOrList cleanUpExpr2
   , strategy      = rationalEquationStrategy
   , ruleOrdering  = ruleOrderingWithId quadraticRuleOrder
   , navigation    = termNavigator
   , examples      = map return (concat brokenEquations)
   }
   
simplifyRationalExercise :: Exercise Expr
simplifyRationalExercise = makeExercise
   { exerciseId   = describe "simplify a rational expression (with a variable in a divisor)" $ 
                       newId "math.simplifyrational"
   , status       = Alpha -- Provisional
   , parser       = parseExpr
-- isSuitable
   , isReady      = isNormBroken
--   , equivalence  = ??
   , similarity   = \x y -> cleanUpExpr2 x == cleanUpExpr2 y
   , strategy     = simplifyRationalStrategy
   , ruleOrdering = ruleOrderingWithId quadraticRuleOrder
   , navigation   = termNavigator
   , examples     = concat (normBroken ++ normBroken2)
   }
   
divisionRationalExercise :: Exercise Expr
divisionRationalExercise = simplifyRationalExercise
   { exerciseId   = describe "divide a rational expression ('uitdelen')" $ 
                       newId "math.divrational"
   , strategy     = label "divide broken fraction" succeed
   , examples     = concat deelUit
   }

rationalEquationStrategy :: LabeledStrategy (Context (OrList (Equation Expr)))
rationalEquationStrategy = cleanUpStrategy (applyTop (fmap (fmap cleanUpExpr2))) $
   label "Rational equation" $ 
       brokenFormToPoly <*> higherDegreeStrategyG <*> checkSolutionStrategy
 where
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
simplifyRationalStrategy = cleanUpStrategy (applyTop cleanUpExpr2) $
   label "Simplify rational expression" $
      phaseOneDiv <*> phaseSimplerDiv
 where
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
commit s = let cs  = cleanUpStrategy (applyTop cleanUpExpr2) (label "" s)
               f a = fromMaybe a (do b <- top a; c <- current a; return (change (const c) b))
           in check (applicable cs . f) <*> s

exceptLowerDiv :: IsStrategy f => f (Context a) -> Strategy (Context a)
exceptLowerDiv = somewhereWith "except-lower-div" $ \a -> 
   if (isDivC a) then [1] else [0 .. arity a-1]

onlyUpperDiv :: IsStrategy f => f (Context a) -> Strategy (Context a)
onlyUpperDiv = onceWith "only-upper-div" $ \a -> [ 1 | isDivC a ]
   
onlyLowerDiv :: IsStrategy f => f (Context a) -> Strategy (Context a)
onlyLowerDiv = onceWith "only-lower-div" $ \a -> [ 2 | isDivC a ]
   
isNormBroken :: Expr -> Bool
isNormBroken expr =
   case expr of
      Negate a -> isNormBroken a
      a :/: b  -> noVarInDivisor a && noVarInDivisor b
      _        -> noVarInDivisor expr

noVarInDivisor :: Expr -> Bool
noVarInDivisor expr = and [ noVars a | _ :/: a <- universe expr ]
                 
brokenEqs :: [Relation Expr] -> OrList (Equation Expr) -> Maybe (OrList Expr)
brokenEqs cs p = do
   xs  <- disjunctions p
   yss <- mapM (brokenEq cs) xs
   return (join (orList yss))

brokenEq :: [Relation Expr] -> Equation Expr -> Maybe (OrList Expr) -- rewrite me
brokenEq cs2 eq = do
   let (lhs :==: rhs) = coverUp eq
       (a, b, cs) = brokenExpr (lhs .-. rhs)
       as = maybe [a] snd (match productView a)
       bs = maybe [b] snd (match productView b)
       new1 = match higherDegreeEquationsView $ orList $ map (:==: 0) as
       new2 = match higherDegreeEquationsView $ orList $ map conv $ 
                 concatMap notZero bs ++ cs2 ++ cs
       conv r = leftHandSide r :==: rightHandSide r
       xs = fromMaybe [] (new2 >>= disjunctions)
   ys <- new1 >>= disjunctions
   return (orList (filter (`notElem` xs) ys))

eqRationalEquation :: Context (OrList (Equation Expr)) -> Context (OrList (Equation Expr)) -> Bool
eqRationalEquation ca cb = fromMaybe False $ do
   a  <- fromContext ca
   b  <- fromContext cb
   let f c = fromMaybe [] $ do
                p  <- conditionOnClipboard c
                qs <- match andView p
                return qs
       csa = f ca
       csb = f cb
   xs <- brokenEqs csa a >>= disjunctions
   ys <- brokenEqs csb b >>= disjunctions
   return (sort xs == sort ys)
   
conditionOnClipboard :: Context a -> Maybe (Logic (Relation Expr))
conditionOnClipboard = evalCM $ const $
   lookupClipboardG "condition"

-- write expression as a/b, under certain conditions
brokenExpr :: Expr -> (Expr, Expr, [Relation Expr])
brokenExpr expr =
   case expr of 
      a :+: b  -> brokenExpr a `fPlus` brokenExpr b
      a :-: b  -> brokenExpr (a :+: Negate b)
      Negate a -> fNeg (brokenExpr a)
      a :*: b  -> brokenExpr a `fTimes` brokenExpr b
      a :/: b  -> brokenExpr a `fTimes` fRecip (brokenExpr b)
      Sym s [a, b] | s == powerSymbol -> 
         fPower (brokenExpr a) b
      _ -> (expr, 1, [])
 where
   fNeg   (a, b, cs)   = (neg a, b, cs)
   fRecip (a, b, cs)   = (b, a, notZero b ++ cs)
   fPower (a, b, cs) n = (a .^. n, b .^. n, cs)
   fTimes (a1, a2, acs) (b1, b2, bcs) = (a1 .*. b1, a2 .*. b2, acs++bcs)
   fPlus  (a1, a2, acs) (b1, b2, bcs) =
      let c2 = lcmExpr a2 b2
          cs = acs++bcs
      in case (divisionExpr c2 a2, divisionExpr c2 b2) of 
            (Just a3, Just b3) 
               | a1 == b1     -> (a1 .*. (a3 .+. b3), c2, cs)
               | a1 == neg b1 -> (a1 .*. (a3 .-. b3), c2, cs)
               | otherwise    -> (a1 .*. a3 .+. b1 .*. b3, c2, cs)
            _ -> (a1 .*. b2 .+. b1 .*. a2, a2 .*. b2, cs)

notZero :: Expr -> [Relation Expr]
notZero expr =
   case match rationalView expr of
      Just r | r /= 0 -> []
      _ -> [expr ./=. 0]

-----------------
-- test code

{-
raar = brokenExpr $ x^2/(5*x+6) + 1
 where x = Var "x"
 
go0 = checkExercise rationalEquationExercise
go2 = checkExercise simplifyRationalExercise
-}
see n = printDerivation ex (examples ex !! (n-1))
 where ex = rationalEquationExercise -- simplifyRationalExercise
  {-     
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