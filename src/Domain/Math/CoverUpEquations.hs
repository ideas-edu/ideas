module Domain.Math.CoverUpEquations (coverUpExercise) where

import Prelude hiding (repeat)
import Control.Monad
-- import Common.Apply
import Common.Context
import Common.Exercise
import Common.Strategy
import Common.Transformation
import Domain.Math.Expr
import Domain.Math.ExercisesDWO
import Domain.Math.Equation
import Domain.Math.QuadraticEquations (solvedList)
import Domain.Math.OrList
import Domain.Math.Parser
import Domain.Math.Views
import Test.QuickCheck (oneof)

{-
main = map test (concat (fillInResult ++ coverUpEquations))

test e = case apply coverUpStrategy (inContext (OrList [e])) of
            Just a | solved (fromContext a)  -> True
                   | otherwise -> error (show (e, a))
            _ -> error (show e) -}

------------------------------------------------------------
-- Exercise

coverUpExercise :: Exercise (OrList (Equation Expr))
coverUpExercise = makeExercise 
   { identifier    = "coverup"
   , domain        = "math"
   , description   = "solve an equation by covering up"
   , status        = Experimental
   , parser        = parseWith (pOrList (pEquation pExpr))
   , equality      = \a b -> a==b
   , equivalence   = \_ _ -> True
   , finalProperty = solvedList
   , ruleset       = map ignoreContext [rule1, rule2, rule3, rule4, rule5, rule6, rule7]
   , strategy      = coverUpStrategy
   , generator     = oneof (map (return . OrList . return) (concat (fillInResult ++ coverUpEquations)))
   }

------------------------------------------------------------
-- Strategy and rules
   
coverUpStrategy :: LabeledStrategy (Context (OrList (Equation Expr)))
coverUpStrategy = label "Cover-up" $ 
   repeat (alternatives $ map ignoreContext [rule1, rule2, rule3, rule4, rule5, rule6, rule7])

rule1, rule2, rule3, rule4, rule5, rule6, rule7 :: Rule (OrList (Equation Expr))
rule1 = makeSimpleRuleList "coverUpPlus"        (toOrs (fmap return . coverUpPlus))
rule2 = makeSimpleRuleList "coverUpTimes"       (toOrs (fmap return . coverUpTimes))
rule3 = makeSimpleRuleList "coverUpNegate"      (toOrs (fmap return . coverUpNegate))
rule4 = makeSimpleRuleList "coverUpNumerator"   (toOrs (fmap return . coverUpNumerator))
rule5 = makeSimpleRuleList "coverUpDenominator" (toOrs (fmap return . coverUpDenominator))
rule6 = makeSimpleRuleList "coverUpSqrt"        (toOrs (fmap return . coverUpSqrt))
rule7 = makeSimpleRuleList "coverUpBase"        (toOrs coverUpBase)

toOrs :: (a -> Maybe [a]) -> OrList a -> [OrList a]
toOrs f (OrList xs) = rec xs 
 where
   rec []     = []
   rec (y:ys) = maybe [] (\zs -> [OrList (zs++ys)]) (f y) ++ map (\(OrList zs) -> OrList (y:zs)) (rec ys)

coverUpPlus :: Equation Expr -> Maybe (Equation Expr)
coverUpPlus (lhs :==: rhs) = do
   c      <- match rationalView rhs
   (a, b) <- match plusView lhs
   case (match rationalView a, match rationalView b) of
      (Just r, _) -> return (b :==: build rationalView (c - r))
      (_, Just r) -> return (a :==: build rationalView (c - r))
      _ -> Nothing

coverUpNegate :: Equation Expr -> Maybe (Equation Expr)
coverUpNegate (Negate a :==: rhs) = do
   c <- match rationalView rhs
   return (a :==: build rationalView (-c))
coverUpNegate _ = Nothing
            
coverUpTimes :: Equation Expr -> Maybe (Equation Expr)
coverUpTimes (lhs :==: rhs) = do
   c      <- match rationalView rhs
   (a, b) <- match timesView lhs
   case (match rationalView a, match rationalView b) of
      (Just r, _) -> return (b :==: build rationalView (c / r))
      (_, Just r) -> return (a :==: build rationalView (c / r))
      _ -> Nothing
      
coverUpNumerator :: Equation Expr -> Maybe (Equation Expr)
coverUpNumerator (lhs :==: rhs) = do
   c      <- match rationalView rhs
   (a, b) <- match (divView >>> second rationalView) lhs
   return (a :==: build rationalView (b*c))
   
coverUpDenominator :: Equation Expr -> Maybe (Equation Expr)
coverUpDenominator (lhs :==: rhs) = do
   c      <- match rationalView rhs
   (a, b) <- match (divView >>> first rationalView) lhs
   return (b :==: build rationalView (a/c))
   
coverUpSqrt :: Equation Expr -> Maybe (Equation Expr)
coverUpSqrt (Sqrt a :==: rhs) = do
   c <- match rationalView rhs
   guard (c >= 0)
   return (a :==: build rationalView (c*c))
coverUpSqrt _ = Nothing

coverUpBase :: Equation Expr -> Maybe [Equation Expr]
coverUpBase (Sym "^" [a, Nat n] :==: rhs) = do
   c <- match rationalView rhs
   let b = round (fromRational c ** (1 / fromIntegral n))
   guard (c >= 0 && product (replicate (fromIntegral n) (fromIntegral b)) == c)
   return $ [ a :==: build integerView b ] ++
            [ a :==: build integerView (-b) | even n ]
coverUpBase _ = Nothing