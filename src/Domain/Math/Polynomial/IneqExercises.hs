-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Domain.Math.Polynomial.IneqExercises
   ( ineqLinearExercise, ineqQuadraticExercise, ineqHigherDegreeExercise
   ) where

import Common.Library hiding (isEmpty)
import Common.Utils.Uniplate (descend)
import Control.Monad
import Data.Foldable (toList)
import Data.Function
import Data.List
import Data.Maybe (fromMaybe)
import Domain.Logic.Formula (Logic((:||:), (:&&:)), catLogic)
import Domain.Math.CleanUp
import Domain.Math.Data.Interval
import Domain.Math.Data.OrList
import Domain.Math.Data.Relation
import Domain.Math.Equation.CoverUpRules hiding (coverUpPlus)
import Domain.Math.Equation.Views
import Domain.Math.Expr
import Domain.Math.Numeric.Views
import Domain.Math.Polynomial.Equivalence
import Domain.Math.Polynomial.Examples
import Domain.Math.Polynomial.Rules
import Domain.Math.Polynomial.Strategies
import Domain.Math.SquareRoot.Views
import qualified Domain.Logic.Formula as Logic
import qualified Domain.Logic.Views as Logic

ineqLinearExercise :: Exercise (Relation Expr)
ineqLinearExercise = makeExercise
   { exerciseId   = describe "solve a linear inequation" $
                       newId "algebra.inequalities.linear"
   , status       = Provisional
   , parser       = parseRelExpr
   , ready        = predicateView relationSolvedForm
   , equivalence  = withoutContext linEq
   , similarity   = withoutContext (viewEquivalent (traverseView cleanUpView))
   , strategy     = ineqLinear
   , navigation   = termNavigator
   , examples     = let x = Var "x"
                        extra = (x-12) / (-2) :>: (x+3)/3
                    in level Medium $
                       map (build inequalityView) (concat ineqLin1 ++ [extra])
   }

ineqQuadraticExercise :: Exercise (Logic (Relation Expr))
ineqQuadraticExercise = makeExercise
   { exerciseId    = describe "solve a quadratic inequation" $
                        newId "algebra.inequalities.quadratic"
   , status        = Provisional
   , parser        = parseLogicRelExpr
   , prettyPrinter = showLogicRelation
   , ready         = predicateView relationsSolvedForm
   , equivalence   = quadrEqContext
   , similarity    = simIneqContext
   , strategy      = ineqQuadratic
   , navigation    = termNavigator
   , ruleOrdering  = ruleOrderingWithId quadraticRuleOrder
   , examples      = level Medium $
                     map (Logic.Var . build inequalityView)
                         (concat $ ineqQuad1 ++ [ineqQuad2, extraIneqQuad])
   }

ineqHigherDegreeExercise :: Exercise (Logic (Relation Expr))
ineqHigherDegreeExercise = makeExercise
   { exerciseId    = describe "solve an inequation of higher degree" $
                        newId "algebra.inequalities.polynomial"
   , status        = Provisional
   , parser        = parseLogicRelExpr
   , prettyPrinter = showLogicRelation
   , ready         = predicateView relationsSolvedForm
   , equivalence   = highEqContext
   , similarity    = simIneqContext
   , strategy      = ineqHigherDegree
   , navigation    = termNavigator
   , ruleOrdering  = ruleOrderingWithId quadraticRuleOrder
   , examples      = level Medium $ map (Logic.Var . build inequalityView) ineqHigh
   }

ineq :: String
ineq = "algebra.inequalities"

simIneqContext :: Context (Logic (Relation Expr)) -> Context (Logic (Relation Expr)) -> Bool
simIneqContext a b =
   sameClipboard a b &&
   withoutContext (simLogic (fmap cleanUpExpr . flipGT)) a b
 where
   sameClipboard = eqExpr `on` lookupClipboardG "ineq"
   eqExpr = (==) :: Maybe Expr -> Maybe Expr -> Bool

--inEquation <- lookupClipboard "ineq" >>= fromExpr

showLogicRelation :: (Eq a, Show a) => Logic (Relation a) -> String
showLogicRelation logic =
   case logic of
      Logic.T     -> "true"
      Logic.F     -> "false"
      Logic.Var a -> show a
      p :||: q    -> showLogicRelation p ++ " or " ++ showLogicRelation q
      p :&&: q    -> case match betweenView logic of
                        Just (x, o1, y, o2, z) ->
                           let f b = if b then "<=" else "<"
                           in unwords [show x, f o1, show y, f o2, show z]
                        _ -> showLogicRelation p ++ " and " ++ showLogicRelation q
      _           -> show logic

betweenView :: Eq a => View (Logic (Relation a)) (a, Bool, a, Bool, a)
betweenView = makeView f h
 where
   f (Logic.Var r1 :&&: Logic.Var r2) = do
      ineq1 <- match inequalityView r1
      ineq2 <- match inequalityView r2
      let g (a :>=: b) = b :<=: a
          g (a :>:  b) = b :<:  a
          g e          = e
      make (g ineq1) (g ineq2)
   f _ = Nothing

   make a b
      | la == rb && ra /= lb = make b a
      | ra == lb =
           Just (la, op a, ra, op b, rb)
      | otherwise = Nothing
    where
      (la, ra) = (leftHandSide a, rightHandSide a)
      (lb, rb) = (leftHandSide b, rightHandSide b)
      op (_ :<=: _) = True
      op _          = False

   h (x, o1, y, o2, z) =
      let g b = if b then (.<=.) else (.<.)
      in Logic.Var (g o1 x y) :&&: Logic.Var (g o2 y z)

ineqLinear :: LabeledStrategy (Context (Relation Expr))
ineqLinear = cleanUpStrategyAfter (applyTop (fmap cleanUpSimple)) ineqLinearG

ineqLinearG :: IsTerm a => LabeledStrategy (Context a)
ineqLinearG = label "Linear inequation" $
   label "Phase 1" (repeatS
       (  use removeDivision
      <|> multi (showId distributeTimes)
             (oncetd (use distributeTimes))
      <|> multi (showId merge) (layer [] (use merge))
       ))
   <*>
   label "Phase 2"
       (  try (use varToLeft)
      <*> try coverUpPlus
      <*> try (use flipSign)
      <*> try (use coverUpTimesPositive)
       )

-- helper strategy (todo: fix needed, because the original rules do not
-- work on relations)
coverUpPlus :: IsTerm a => Strategy (Context a)
coverUpPlus = alternatives (map (use . ($ oneVar)) coverUps)
 where
   coverUps :: [ConfigCoverUp -> Rule (Relation Expr)]
   coverUps =
      [ coverUpBinaryRule "plus" (commOp . isPlus) (-)
      , coverUpBinaryRule "minus-left" isMinus (+)
      , coverUpBinaryRule "minus-right" (flipOp . isMinus) (flip (-))
      ]

coverUpTimesPositive :: Rule (Relation Expr)
coverUpTimesPositive = coverUpBinaryRule "times-positive" (commOp . m) (/) configCoverUp
 where
   m expr = do
      (a, b) <- matchM timesView expr
      r <- matchM rationalView a
      guard (r>0)
      return (a, b)

flipSign :: Rule (Relation Expr)
flipSign = describe "Flip sign of inequality" $
   ruleMaybe (ineq, "flip-sign") $ \r -> do
   let lhs = leftHandSide r
       rhs = rightHandSide r
   guard (isNegative lhs)
   return $ constructor (flipSides r) (neg lhs) (neg rhs)
 where
   isNegative (Negate _) = True
   isNegative expr =
      maybe False fst (match productView expr)

ineqQuadratic :: LabeledStrategy (Context (Logic (Relation Expr)))
ineqQuadratic = cleanUpStrategyAfter (applyTop cleanUpLogicRelation) $
   label "Quadratic inequality" $
      use trivialRelation
       |> try (useC turnIntoEquation)
      <*> quadraticStrategyG
      <*> useC solutionInequation

ineqHigherDegree :: LabeledStrategy (Context (Logic (Relation Expr)))
ineqHigherDegree = cleanUpStrategyAfter (applyTop cleanUpLogicRelation) $
   label "Inequality of a higher degree" $
      use trivialRelation
       |> try (useC turnIntoEquation)
      <*> higherDegreeStrategyG
      <*> useC solutionInequation

-- First, cleanup expression. Then, cleanup equations only (there is an
-- explicit rule for the other relations). Finally, simplify the logical
-- proposition (including impotency or).
cleanUpLogicRelation :: Logic (Relation Expr) -> Logic (Relation Expr)
cleanUpLogicRelation =
   let f a | relationType a == EqualTo = build orListView (cleanUpRelation a)
           | otherwise                 = Logic.Var a
   in simplifyWith noDuplicates orListView . Logic.simplify
    . catLogic . fmap (f . fmap cleanUpExpr)

trivialRelation :: Rule (OrList (Relation Expr))
trivialRelation =
   ruleMaybe (ineq, "trivial") $ oneDisjunct $ \a -> do
      let new = cleanUpRelation a
      guard (isTrue new || isFalse new)
      return new

turnIntoEquation :: Rule (Context (Relation Expr))
turnIntoEquation = describe "Turn into equation" $
   ruleMaybe (ineq, "to-equation") $ \cr -> do
   r <- currentInContext cr
   guard (relationType r `elem` ineqTypes)
   return $ addToClipboard "ineq" (toExpr r)
          $ replaceInContext (leftHandSide r .==. rightHandSide r) cr
 where
   ineqTypes =
      [LessThan, GreaterThan, LessThanOrEqualTo, GreaterThanOrEqualTo]

-- Todo: cleanup this function
solutionInequation :: Rule (Context (Logic (Relation Expr)))
solutionInequation = describe "Determine solution for inequality" $
   makeRule (ineq, "give-solution") $ \clr -> do
   r <- currentInContext clr
   inEquation <- lookupClipboardG "ineq" clr
   let rt = relationType inEquation
   orv  <- matchM orListView r
   new <- case toList orv of
      _ | isTrue orv ->
         return $ fromBool $
            rt `elem` [GreaterThanOrEqualTo, LessThanOrEqualTo]
      _ | isFalse orv -> do -- no solutions found for equations
         let vs = vars (toExpr inEquation)
         guard (not (null vs))
         return $ fromBool $ evalIneq inEquation (head vs) 0
      xs -> do
         (vs, ys) <- liftM unzip $ matchM (listView (equationView >>> equationSolvedForm)) xs
         let v  = head vs
             zs = nub $ map (simplify (squareRootViewWith rationalView)) ys
         ds <- matchM (listView doubleView) zs
         guard (all (==v) vs)
         let rs = makeRanges including (sort (zipWith A ds zs))
             including = rt `elem` [GreaterThanOrEqualTo, LessThanOrEqualTo]
         return $ fmap (fmap fromDExpr) $ intervalRelations (A 0 (Var v)) $
            ors [ this | (d, isP, this) <- rs, isP || evalIneq inEquation v d ]
   return $ removeClipboard "ineq"
          $ replaceInContext new clr
 where
   makeRanges :: Bool -> [DExpr] -> [(Double, Bool, Interval DExpr)]
   makeRanges b xs =
      [makeLeft $ head xs]
      ++ concatMap (uncurry makeMiddle) (zip xs (drop 1 xs))
      ++ [makePoint (last xs) | b]
      ++ [makeRight $ last xs]
    where
      makeLeft  a@(A d _)
         | b         = (d-1, False, lessThanOrEqualTo a)
         | otherwise = (d-1, False, lessThan a)
      makeRight a@(A d _)
         | b         = (d+1, False, greaterThanOrEqualTo a)
         | otherwise = (d+1, False, greaterThan a)
      makePoint a@(A d _) = (d, True, point a)
      makeMiddle a1@(A d1 _) a2@(A d2 _) =
         [ makePoint a1 | b ] ++
         [ ( (d1+d2)/2
           , False
           , open a1 a2
           )
         ]

   evalIneq :: Relation Expr -> String -> Double -> Bool
   evalIneq r v d = fromMaybe False $
      liftM2 (eval (relationType r)) (useSide leftHandSide) (useSide rightHandSide)
    where
      useSide f = match doubleView (sub (f r))

      sub (Var x) | x==v = fromDouble d
      sub expr = descend sub expr

data DExpr = A Double Expr

instance Eq DExpr where
   A d1 _ == A d2 _ = d1==d2

instance Ord DExpr where
   A d1 _ `compare` A d2 _ = d1 `compare` d2

fromDExpr :: DExpr -> Expr
fromDExpr (A _ e) = e