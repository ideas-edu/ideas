module Domain.Math.Strategy.BrokenEquations where

import Prelude hiding (repeat)
import Common.Apply
import Common.Strategy
import Common.Transformation
import Common.Traversable
import Control.Monad
import Data.Ratio
import Domain.Math.Expr
import Domain.Math.Data.Equation
import Domain.Math.Data.OrList
import Domain.Math.View.Basic
import Domain.Math.ExercisesDWO
import Domain.Math.Strategy.HigherDegreeEquations (equationsStrategy)
import Domain.Math.Strategy.QuadraticEquations (solvedList, polyView)
import Domain.Math.Strategy.CoverUpEquations (rule1)
{- 
Equations:
   (a/b) = 0       =   a=0
   (a/b) = 1       =   a=b
   (a/c) = (b/c)   =   a==b
   (a/b) = (a/c)   =   b==c
-}

main = flip mapM_ (concat brokenEquations) $ \eq -> 
   let res = applyD brokenStrategy (OrList [eq])
   in if solvedList res then print "ok" else print res

-- TODO: check results afterwards (possibility of division by zero)
brokenStrategy :: LabeledStrategy (OrList (Equation Expr))
brokenStrategy = label "broken" $ notBroken <*> 
   try (check p <*> equationsStrategy <*> repeat inconsistencies)
 where p (OrList xs) = all (`belongsTo` polyView) $ concat [[a,b] | a:==: b <- xs]

notBroken :: LabeledStrategy (OrList (Equation Expr))
notBroken = label "not broken" $ repeat $ 
   alternatives [brokenZero, brokenOne, brokenSameDenom, brokenSameNum]
   |> alternatives [coverUpPlus, brokenCross]

coverUpPlus = rule1

------------------------------------------------------------------------
-- Equation rules

brokenZero :: Rule (OrList (Equation Expr))
brokenZero = makeSimpleRule "brokenZero" $ onceJoinM $ \(a :==: b) -> do
   n <- match rationalView b
   guard (n==0)
   (x, _) <- match divView a
   return $ OrList [x :==: 0]
 `mplus` do
   n <- match rationalView a
   guard (n==0)
   (x, _) <- match divView b
   return $ OrList [x :==: 0]

brokenOne :: Rule (OrList (Equation Expr))
brokenOne = makeSimpleRule "brokenOne" $ onceJoinM $ \(a :==: b) -> do
   n <- match rationalView b
   guard (n==1)
   (x, y) <- match divView a
   return $ OrList [x :==: y]
 `mplus` do
   n <- match rationalView a
   guard (n==1)
   (x, y) <- match divView b
   return $ OrList [x :==: y]

brokenSameDenom :: Rule (OrList (Equation Expr))
brokenSameDenom = makeSimpleRule "brokenSameDenom" $ onceJoinM $ \(a :==: b) -> do
   (x1, y1) <- match divView a
   (x2, y2) <- match divView b
   guard (y1==y2)
   return $ OrList [x1 :==: x2]

brokenSameNum :: Rule (OrList (Equation Expr))
brokenSameNum = makeSimpleRule "brokenSameNum" $ onceJoinM  $ \(a :==: b) -> do
   (x1, y1) <- match divView a
   (x2, y2) <- match divView b
   guard (x1==x2)
   return $ OrList [y1 :==: y2]

brokenCross :: Rule (OrList (Equation Expr))
brokenCross = makeSimpleRule "brokenCross" $ onceJoinM $ \(a :==: b) -> do
   let matchDiv e = match divView e `mplus` fmap f (match rationalView e)
       f r = (fromInteger (numerator r), fromInteger (denominator r))
   (x1, y1) <- matchDiv a
   (x2, y2) <- matchDiv b
   return $ OrList[x1 .*. y2 :==: x2 .*. y1]

-- remove inconsistent equations from the or-list, such as 0==1
-- TODO: move this to a different module (should not be here)
inconsistencies :: Rule (OrList (Equation Expr))
inconsistencies = makeSimpleRule "inconsistencies" $ onceJoinM $ \(a :==: b) -> do
   r1 <- match rationalView a
   r2 <- match rationalView b
   guard (r1 /= r2)
   return $ OrList []